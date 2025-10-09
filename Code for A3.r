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
  m.ret, dl.dlret, dl.dlretx
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
  dplyr::select(date, permno, shrcd, exchcd, ret, dlret, dlretx)

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
