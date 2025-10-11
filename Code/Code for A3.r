install.packages(c("RPostgres","dplyr","tidyr","lubridate","zoo","stringr","readr","fredr"))
library(RPostgres)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(stringr)
library(readr)
library(fredr)




wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='harrisonvu')


has_col <- function(con, schema, table, column){
  sql <- sprintf("
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema='%s' AND table_name='%s' AND column_name='%s'
    LIMIT 1", schema, table, column)
  nrow(DBI::dbGetQuery(con, sql)) > 0
}
endcol <- if (has_col(wrds, "crsp_a_stock", "msenames", "nameenddt")) "nameenddt" else "nameendt"




qry <- sprintf("
WITH msf_base AS (
  SELECT permno, date, ret
  FROM crsp_a_stock.msf
  WHERE date BETWEEN '1926-01-01' AND '1968-06-30'
),
names AS (
  SELECT permno, shrcd, exchcd, namedt, COALESCE(%s, DATE '9999-12-31') AS nameend
  FROM crsp_a_stock.msenames
),
dlist AS (
  SELECT permno, dlstdt, dlret, dlretx
  FROM crsp_a_stock.dsedelist
)
SELECT
  m.permno, m.date,
  CAST(TO_CHAR(m.date, 'YYYYMM') AS INTEGER) AS yyyymm,
  n.shrcd, n.exchcd,
  m.ret, dl.dlret, dl.dlretx,  dl.dlstdt
FROM msf_base m
JOIN names n
  ON m.permno = n.permno
 AND m.date BETWEEN n.namedt AND n.nameend
LEFT JOIN dlist dl
  ON m.permno = dl.permno
 AND date_trunc('month', m.date) = date_trunc('month', dl.dlstdt)
WHERE n.exchcd = 1
  AND n.shrcd IN (10, 11)
ORDER BY m.date, m.permno
", endcol)


crsp_m <- dbGetQuery(wrds, qry) %>%
  dplyr::select(date, permno, shrcd, exchcd, ret, dlret, dlretx, dlstdt)

# ============================
# 3) Fisher’s arithmetic index (Equal-weighted NYSE return)
# ============================
fisher_eqw <- crsp_m %>%
  mutate(month = as.yearmon(as.Date(date))) %>%  
  group_by(month) %>%
  summarise(
    n_nyse      = sum(!is.na(ret)),
    mkt_eqw_ret = mean(as.numeric(ret), na.rm = TRUE),
    .groups = "drop"
  )


# ============================
# --- 3) Fama–French monthly RF from WRDS ---
qry_rf <- "
SELECT date, rf
FROM ff.factors_monthly
WHERE date BETWEEN '1926-01-01' AND '1968-06-30'
ORDER BY date
"
ff_rf <- DBI::dbGetQuery(wrds, qry_rf) %>%
  mutate(
    month       = as.yearmon(as.Date(date)),
    rf_m = rf / 100
  ) %>%
  select(month, rf_m)

# --- 4) Merge Market Index with Risk-Free ---
mkt_rf <- fisher_eqw %>%
  inner_join(ff_rf, by = "month") %>%
  mutate(date = as.Date(month)) %>%             
  select(date, n_nyse, mkt_eqw_ret, rf_m) %>%
  arrange(date)



#--------------Clean and Modification Process



# ============ 1) Clean CRSP:  Adjust for delistings and mising values ============
crsp_clean <- crsp_m %>%
  mutate(
    # standardize dates + build monthly key
    date        = as.Date(date),
    dlstdt      = suppressWarnings(as.Date(dlstdt)),      #  # NA if not delisted
    month       = as.yearmon(date),
    delist_m    = ifelse(is.na(dlstdt), NA, as.yearmon(dlstdt)),
    # safe numeric coercion
    ret_num     = suppressWarnings(as.numeric(ret)),
    dlret_num   = suppressWarnings(as.numeric(dlret))
  ) %>%
  # use dlret ONLY in the delist month; 0 otherwise
  mutate(
    dlret_eff   = ifelse(!is.na(delist_m) & month == delist_m, dlret_num, 0),
    # combined monthly return with delisting (if any)
    ret_adj     = (1 + coalesce(ret_num, 0)) * (1 + coalesce(dlret_eff, 0)) - 1
  ) %>%
  # remove any observations AFTER the delist month
  group_by(permno) %>%
  filter(is.na(delist_m) | month <= delist_m) %>%
  ungroup() %>%
  # keep a tidy set of columns; date first
  select(date, month, permno, shrcd, exchcd, ret, dlret, dlretx, ret_adj)

# Interpretation of missing values (as implemented):
# - If ret is NA and it is NOT the delist month -> keep NA (do not set 0).
# - If ret is NA and it IS the delist month with dlret present -> ret_adj equals dlret via the formula.
# - Any rows after dlstdt are dropped.

# ============ 2) Fisher equal-weighted (use the adjusted returns) ============
fisher_eqw <- crsp_clean %>%
  group_by(month) %>%
  summarise(
    # number of stocks contributing that month
    n_nyse      = sum(!is.na(ret_adj)),
    mkt_eqw_ret = mean(ret_adj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(month)) %>%
  select(date, month, n_nyse, mkt_eqw_ret)

# ============ 3) Risk-free from WRDS Fama–French ============
qry_rf <- "
SELECT date, rf
FROM ff.factors_monthly
WHERE date BETWEEN '1926-01-01' AND '1968-06-30'
ORDER BY date
"
ff_rf <- DBI::dbGetQuery(wrds, qry_rf) %>%
  mutate(
    month       = as.yearmon(as.Date(date)),
    rf_m_simple = rf / 100
  ) %>%
  select(month, rf_m_simple)

# ============ 4) Merge Market + RF month ============
mkt_rf <- fisher_eqw %>%
  inner_join(ff_rf, by = "month") %>%
  arrange(date) %>%
  select(date, n_nyse, mkt_eqw_ret, rf_m_simple)




