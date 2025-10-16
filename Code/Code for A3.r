#======================================================
# Fama & MacBeth (1973) Replication
# Step 1: Data Extraction and Cleaning script
#======================================================

#Environment Steup
install.packages(c("RPostgres","dplyr","tidyr","lubridate","zoo","stringr","readr","fredr"))
library(RPostgres)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(stringr)
library(readr)
library(fredr)



# Wrds connection
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')

#Raw Data Extraction. Helps to ensure the script wont fail due to database changes in the future.
has_col <- function(con, schema, table, column){
  sql <- sprintf("
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema='%s' AND table_name='%s' AND column_name='%s'
    LIMIT 1", schema, table, column)
  nrow(DBI::dbGetQuery(con, sql)) > 0
}
endcol <- if (has_col(wrds, "crsp_a_stock", "msenames", "nameenddt")) "nameenddt" else "nameendt"



# This query fetches all necessary data: monthly returns,share codes and delisting returns
qry <- sprintf("
  SELECT
    m.permno, m.date, m.ret,
    n.shrcd, n.exchcd,
    d.dlret, d.dlstdt
  FROM crsp_a_stock.msf AS m
  JOIN crsp_a_stock.msenames AS n
    ON m.permno = n.permno AND m.date BETWEEN n.namedt AND COALESCE(n.%s, '9999-12-31')
  LEFT JOIN crsp_a_stock.dsedelist AS d
    ON m.permno = d.permno AND date_trunc('month', m.date) = date_trunc('month', d.dlstdt)
  WHERE n.exchcd = 1
    AND n.shrcd IN (10, 11)
    AND m.date BETWEEN '1926-01-01' AND '1968-06-30'
  ORDER BY m.date, m.permno
", endcol)

crsp_m <- dbGetQuery(wrds, qry)



#======================================================
# Fama & MacBeth (1973) Replication
# Step 2: Clean CRSP Data, Adjust for delistings and missing values
#======================================================

#Step 2.1 - Creating the adjusted return column(ret_adj)
crsp_clean <- crsp_m %>%
  mutate(
    date        = as.Date(date),
    dlstdt      = suppressWarnings(as.Date(dlstdt)),
    month       = as.yearmon(date),
    delist_m    = ifelse(is.na(dlstdt), NA, as.yearmon(dlstdt)),
    ret_num     = suppressWarnings(as.numeric(ret)),
    dlret_num   = suppressWarnings(as.numeric(dlret))
  ) %>%
  mutate(
    dlret_eff   = ifelse(!is.na(delist_m) & month == delist_m, dlret_num, 0),
    ret_adj     = (1 + coalesce(ret_num, 0)) * (1 + coalesce(dlret_eff, 0)) - 1
  ) %>%
  group_by(permno) %>%
  filter(is.na(delist_m) | month <= delist_m) %>%
  ungroup() %>%
  select(date, month, permno, ret_adj)



#Step 2.2 -  Calculating equal weighted market return(Fisher Index)
fisher_eqw <- crsp_clean %>%
  group_by(month) %>%
  summarise(
    n_nyse      = sum(!is.na(ret_adj)),
    mkt_eqw_ret = mean(ret_adj, na.rm = TRUE),
    .groups = "drop"
  )





#======================================================
# Fama & MacBeth (1973) Replication
# Step 3: Fetch risk free rate from wrds
#======================================================

qry_rf <- "
  SELECT date, rf
  FROM ff.factors_monthly
  WHERE date BETWEEN '1926-01-01' AND '1968-06-30'
  ORDER BY date
"
ff_rf <- DBI::dbGetQuery(wrds, qry_rf) %>%
  mutate(
    month = as.yearmon(as.Date(date)),
    rf_m  = rf / 100
  ) %>%
  select(month, rf_m)

#======================================================
# Fama & MacBeth (1973) Replication
# Step 4: Merge market index with risk free rate
#======================================================


# creates the analysis-ready market data frame.
mkt_rf <- fisher_eqw %>%
  inner_join(ff_rf, by = "month") %>%
  mutate(date = as.Date(month)) %>%
  select(date, month, n_nyse, mkt_eqw_ret, rf_m) %>%
  arrange(date)

# --- Disconnect from WRDS ---
dbDisconnect(wrds)

print("Data preparation complete. The 'crsp_clean' and 'mkt_rf' dataframes are ready for analysis.")






















