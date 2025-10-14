
# TABLE 2 — sample statistics for four selected estimation periods
# =========================

library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(broom)

# --- market (equal-weight NYSE) by month from your pipeline


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
  select(date, n_nyse, mkt_eqw_ret, rf_m) %>%
  arrange(date)




mkt_by_month <- fisher_eqw %>%
  transmute(month, Rm = mkt_eqw_ret)

# --- stock-month panel with delisting-adjusted returns joined to market
stock_m <- crsp_clean %>%
  select(permno, month, Ri = ret_adj) %>%
  inner_join(mkt_by_month, by = "month")

# --- helpers ---------------------------------------------------------------

# per-security market model on a window -> beta_i and s(eps_i)
fit_mm_security <- function(df) {
  # df must have Ri, Rm
  if (nrow(df) < 12 || any(!is.finite(df$Ri)) || any(!is.finite(df$Rm))) {
    return(tibble(beta_i = NA_real_, s_eps_i = NA_real_))
  }
  fit <- lm(Ri ~ Rm, data = df)
  tibble(
    beta_i  = unname(coef(fit)[["Rm"]]),
    s_eps_i = sd(residuals(fit))
  )
}

# portfolio market model on series -> r2, s(Rp), s(eps_p)
fit_mm_portfolio <- function(port_ret_est, mkt_est) {
  mm   <- lm(port_ret_est ~ mkt_est)
  r2   <- summary(mm)$r.squared
  sRp  <- sd(port_ret_est)
  s_ep <- sd(residuals(mm))
  tibble(r2 = r2, sRp = sRp, s_ep = s_ep)
}

# sizing rule for 20 portfolios (paper: middle 18 = floor(N/20), remainder split 1 & 20)
sizes_for_20 <- function(N) {
  base <- floor(N/20)
  r    <- N - 20*base
  add1 <- floor(r/2)
  add20<- ceiling(r/2)
  sizes <- rep(base, 20)
  if (r > 0) {
    sizes[1]  <- sizes[1]  + add1
    sizes[20] <- sizes[20] + add20
  }
  sizes
}

# order of statistics as printed
.stat_order <- c(
  "β̂_{p,t−1}",
  "s(β̂_{p,t−1})",
  "r(R_p, R_m)^2",
  "s(R_p)",
  "s(ε̂_p)",
  "s̄_{p,t−1}(ε_i)",
  "s(ε̂_p)/s̄_{p,t−1}(ε_i)"
)

# format one half-table (1–10 or 11–20) with a top "PERIODS" row
make_half_table <- function(stats_tbl, idx) {
  wide <- stats_tbl %>%
    dplyr::filter(Portfolio %in% idx) %>%
    dplyr::arrange(Portfolio) %>%
    tidyr::pivot_longer(-Portfolio, names_to = "Statistic", values_to = "Value") %>%
    dplyr::mutate(Statistic = factor(Statistic, levels = .stat_order)) %>%
    dplyr::arrange(Statistic, Portfolio) %>%
    dplyr::select(Statistic, Portfolio, Value) %>%
    tidyr::pivot_wider(names_from = Portfolio, values_from = Value)
  
  # column names like the paper
  names(wide)[1]  <- " "
  names(wide)[-1] <- as.character(idx)
  
  # <<< make all columns character so bind_rows() works with the header
  wide <- wide %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # prepend the "PERIODS" row
  header <- tibble::as_tibble_row(
    setNames(as.list(c("Statistic", rep("", length(idx)))), names(wide))
  )
  dplyr::bind_rows(header, wide)
}
# --- core worker for 1 formation/estimation/testing triplet ----------------

table2_for_triplet <- function(formation_start, formation_end,
                               est_start, est_end,
                               test_first_year, test_first_month = 1) {
  
  ym_form <- seq(as.yearmon(paste0(formation_start, "-01")),
                 as.yearmon(paste0(formation_end,   "-12")), by = 1/12)
  ym_est  <- seq(as.yearmon(paste0(est_start,      "-01")),
                 as.yearmon(paste0(est_end,        "-12")), by = 1/12)
  ym_test1 <- as.yearmon(paste0(test_first_year, "-", sprintf("%02d", test_first_month)))
  
  # Universe: present in first test month, full 60 est months, >=48 form months
  universe <- stock_m %>%
    mutate(in_form = month %in% ym_form,
           in_est  = month %in% ym_est) %>%
    group_by(permno) %>%
    summarise(
      has_test1 = any(month == ym_test1 & is.finite(Ri)),
      n_est     = sum(in_est  & is.finite(Ri)),
      n_form    = sum(in_form & is.finite(Ri)),
      .groups = "drop"
    ) %>%
    filter(has_test1, n_est == length(ym_est), n_form >= 48) %>%
    pull(permno)
  
  if (length(universe) < 20) stop("Too few securities in universe for this period.")
  
  # Formation betas -> rank into 20 portfolios
  betas_form <- stock_m %>%
    filter(permno %in% universe, month %in% ym_form) %>%
    group_by(permno) %>%
    group_modify(~ fit_mm_security(.x)) %>%
    ungroup() %>%
    filter(is.finite(beta_i)) %>%
    arrange(beta_i)
  
  sizes <- sizes_for_20(nrow(betas_form))
  betas_form <- betas_form %>%
    mutate(port = rep(1:20, times = sizes))
  membership <- betas_form %>% select(permno, port)
  
  # Estimation-window security stats (β_i, s(eps_i)) and portfolio membership
  sec_est <- stock_m %>%
    filter(permno %in% membership$permno, month %in% ym_est) %>%
    group_by(permno) %>%
    group_modify(~ fit_mm_security(.x)) %>%
    ungroup() %>%
    inner_join(membership, by = "permno")
  
  # Portfolio return series (equal-weight) over estimation window
  port_month_est <- stock_m %>%
    filter(permno %in% membership$permno, month %in% ym_est) %>%
    inner_join(membership, by = "permno") %>%
    group_by(port, month) %>%
    summarise(Rp = mean(Ri, na.rm = TRUE), .groups = "drop") %>%
    arrange(port, month) %>%
    inner_join(mkt_by_month %>% filter(month %in% ym_est), by = "month")
  
  # Portfolio market-model fits
  port_fit <- port_month_est %>%
    group_by(port) %>%
    summarise(fit = list(fit_mm_portfolio(Rp, Rm)), .groups = "drop") %>%
    unnest(fit)
  
  # s(β̂_{p,t-1}) = s(ε̂_p) / (sqrt(n) * s(R_m))
  n_est <- length(ym_est)
  sRm   <- stock_m %>% filter(month %in% ym_est) %>%
    distinct(month, Rm) %>% pull(Rm) %>% sd()
  port_fit <- port_fit %>% mutate(s_beta = s_ep / (sqrt(n_est) * sRm))
  
  # β̂_{p,t-1} and s̄_{p,t-1}(ε_i) as portfolio averages of security stats
  beta_bar <- sec_est %>% group_by(port) %>% summarise(beta_bar   = mean(beta_i,  na.rm = TRUE), .groups = "drop")
  sbar_eps <- sec_est %>% group_by(port) %>% summarise(sbar_eps_i = mean(s_eps_i, na.rm = TRUE), .groups = "drop")
  
  # Collect seven statistics, round like the paper
  stats <- beta_bar %>%
    inner_join(port_fit, by = "port") %>%
    inner_join(sbar_eps, by = "port") %>%
    transmute(
      Portfolio                = port,
      `β̂_{p,t−1}`              = round(beta_bar, 3),
      `s(β̂_{p,t−1})`           = round(s_beta,   3),
      `r(R_p, R_m)^2`          = round(r2,       3),
      `s(R_p)`                 = round(sRp,      3),
      `s(ε̂_p)`                 = round(s_ep,     3),
      `s̄_{p,t−1}(ε_i)`         = round(sbar_eps_i, 3),
      `s(ε̂_p)/s̄_{p,t−1}(ε_i)` = round(s_ep / sbar_eps_i, 3)
    ) %>%
    arrange(Portfolio)
  
  # Build the two halves (1–10, 11–20)
  list(
    panel_1to10  = make_half_table(stats, 1:10),
    panel_11to20 = make_half_table(stats, 11:20)
  )
}

# --- run for the four estimation periods shown in Table 2 ------------------

runs <- list(
  `1934–38` = table2_for_triplet(formation_start = 1927, formation_end = 1933,
                                 est_start = 1934, est_end = 1938,
                                 test_first_year = 1939, test_first_month = 1),
  `1942–46` = table2_for_triplet(formation_start = 1935, formation_end = 1941,
                                 est_start = 1942, est_end = 1946,
                                 test_first_year = 1947, test_first_month = 1),
  `1950–54` = table2_for_triplet(formation_start = 1943, formation_end = 1949,
                                 est_start = 1950, est_end = 1954,
                                 test_first_year = 1955, test_first_month = 1),
  `1958–62` = table2_for_triplet(formation_start = 1951, formation_end = 1957,
                                 est_start = 1958, est_end = 1962,
                                 test_first_year = 1963, test_first_month = 1)
)

# ---  print the panel for portfolio 1 to 10
table2_1934_38_A <- runs[[ "1934–38" ]]$panel_1to10
table2_1942_46_A <- runs[["1942–46"]]$panel_1to10
table2_1950_54_A <- runs[["1950–54"]]$panel_1to10
table2_1958_62_A <- runs[["1958–62"]]$panel_1to10

print(as.data.frame(table2_1934_38_A), row.names = FALSE)
print(as.data.frame(table2_1942_46_A), row.names = FALSE)
print(as.data.frame(table2_1950_54_A), row.names = FALSE)
print(as.data.frame(table2_1958_62_A), row.names = FALSE)

# ---  print the panel for portfolio 11 to 20
table2_1934_38_B <- runs[[ "1934–38" ]]$panel_11to20
table2_1942_46_B <- runs[["1942–46"]]$panel_11to20
table2_1950_54_B <- runs[["1950–54"]]$panel_11to20
table2_1958_62_B <- runs[["1958–62"]]$panel_11to20


print(as.data.frame(table2_1934_38_B), row.names = FALSE)
print(as.data.frame(table2_1942_46_B), row.names = FALSE)
print(as.data.frame(table2_1950_54_B), row.names = FALSE)
print(as.data.frame(table2_1958_62_B), row.names = FALSE) 




