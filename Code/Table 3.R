#====================================================================
# Fama & MacBeth (1973) Replication
# Analysis Part: Portfolio Formation, Beta Estimation,
# Cross-Sectional Regressions, and Formatted Excel Export
#====================================================================

# --- Step 1: Load Packages ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, zoo, broom, knitr, purrr, lubridate, openxlsx)

# --- Step 2: Define Periods ---
# This defines the 9 rolling cycles from Table 1 of the paper.
cycles <- tribble(
  ~cycle, ~form_start, ~form_end, ~est_start, ~est_end, ~test_start, ~test_end,
  1,      "1926-01",   "1929-12", "1930-01",  "1934-12", "1935-01",   "1938-12",
  2,      "1927-01",   "1933-12", "1934-01",  "1938-12", "1939-01",   "1942-12",
  3,      "1931-01",   "1937-12", "1938-01",  "1942-12", "1943-01",   "1946-12",
  4,      "1935-01",   "1941-12", "1942-01",  "1946-12", "1947-01",   "1950-12",
  5,      "1939-01",   "1945-12", "1946-01",  "1950-12", "1951-01",   "1954-12",
  6,      "1943-01",   "1949-12", "1950-01",  "1954-12", "1955-01",   "1958-12",
  7,      "1947-01",   "1953-12", "1954-01",  "1958-12", "1959-01",   "1962-12",
  8,      "1951-01",   "1957-12", "1958-01",  "1962-12", "1963-01",   "1966-12",
  9,      "1955-01",   "1961-12", "1962-01",  "1966-12", "1967-01",   "1968-06"
) %>%
  mutate(across(form_start:test_end, as.yearmon))

# --- Step 3: Helper Functions (from your provided script) ---
# These modular functions accurately implement the F-M methodology.
estimate_betas <- function(returns_df, market_df, start_period, end_period) {
  period_data <- returns_df %>%
    filter(month >= start_period & month <= end_period) %>%
    inner_join(market_df %>% select(month, mkt_eqw_ret), by = "month")
  obs_counts <- period_data %>%
    group_by(permno) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    filter(n_obs >= 24)
  betas <- period_data %>%
    semi_join(obs_counts, by = "permno") %>%
    group_by(permno) %>%
    do(tryCatch({
      model <- lm(ret_adj ~ mkt_eqw_ret, data = .)
      tibble(beta = coef(model)[2], resid_sd = sd(residuals(model), na.rm = TRUE))
    }, error = function(e) tibble(beta = NA, resid_sd = NA))) %>%
    ungroup() %>%
    filter(!is.na(beta))
  return(betas)
}
form_portfolios <- function(betas_df, n_portfolios = 20) {
  ranked <- betas_df %>%
    arrange(beta) %>%
    mutate(rank = row_number())
  n_securities <- nrow(ranked)
  base_size <- floor(n_securities / n_portfolios)
  remainder <- n_securities %% n_portfolios
  ranked <- ranked %>%
    mutate(portfolio = case_when(
      rank <= base_size + ceiling(remainder / 2) ~ 1,
      rank > n_securities - (base_size + floor(remainder / 2)) ~ n_portfolios,
      TRUE ~ ceiling((rank - (base_size + ceiling(remainder / 2))) / base_size) + 1
    ))
  return(ranked %>% select(permno, portfolio))
}
calc_portfolio_stats <- function(portfolio_assignments, returns_df, market_df, start_period, end_period) {
  est_data <- returns_df %>%
    filter(month >= start_period & month <= end_period) %>%
    inner_join(portfolio_assignments, by = "permno") %>%
    inner_join(market_df %>% select(month, mkt_eqw_ret), by = "month")
  obs_counts <- est_data %>%
    group_by(permno, portfolio) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    filter(n_obs >= 24)
  indiv_stats <- est_data %>%
    semi_join(obs_counts, by = c("permno", "portfolio")) %>%
    group_by(permno, portfolio) %>%
    do(tryCatch({
      model <- lm(ret_adj ~ mkt_eqw_ret, data = .)
      tibble(beta_i = coef(model)[2], resid_sd_i = sd(residuals(model), na.rm = TRUE))
    }, error = function(e) tibble(beta_i = NA, resid_sd_i = NA))) %>%
    ungroup() %>%
    filter(!is.na(beta_i))
  port_stats <- indiv_stats %>%
    group_by(portfolio) %>%
    summarise(
      beta_p = mean(beta_i, na.rm = TRUE),
      beta_sq_p = mean(beta_i^2, na.rm = TRUE),
      resid_sd_p = mean(resid_sd_i, na.rm = TRUE),
      .groups = "drop"
    )
  return(list(portfolio_stats = port_stats, individual_stats = indiv_stats))
}
update_betas_monthly <- function(portfolio_assignments, individual_betas, returns_df, market_df, test_month, est_start_year) {
  should_update <- month(as.Date(test_month)) == 1
  if (should_update && !is.null(est_start_year)) {
    update_start <- as.yearmon(paste0(est_start_year, "-01"))
    est_end <- test_month - 1 / 12
    updated_betas <- returns_df %>%
      filter(month >= update_start & month <= est_end) %>%
      inner_join(portfolio_assignments, by = "permno") %>%
      inner_join(market_df %>% select(month, mkt_eqw_ret), by = "month") %>%
      group_by(permno, portfolio) %>%
      do(tryCatch({
        model <- lm(ret_adj ~ mkt_eqw_ret, data = .)
        tibble(beta_i = coef(model)[2], resid_sd_i = sd(residuals(model), na.rm = TRUE))
      }, error = function(e) tibble(beta_i = NA, resid_sd_i = NA))) %>%
      ungroup() %>%
      filter(!is.na(beta_i))
    individual_betas <- updated_betas
  }
  active_securities <- returns_df %>%
    filter(month == test_month) %>%
    select(permno)
  monthly_stats <- individual_betas %>%
    inner_join(active_securities, by = "permno") %>%
    group_by(portfolio) %>%
    summarise(
      beta_p = mean(beta_i, na.rm = TRUE),
      beta_sq_p = mean(beta_i^2, na.rm = TRUE),
      resid_sd_p = mean(resid_sd_i, na.rm = TRUE),
      .groups = "drop"
    )
  return(list(monthly_stats = monthly_stats, updated_individual_betas = individual_betas))
}
run_fama_macbeth <- function(cycle_num, cycles_df, returns_df, market_df) {
  cycle_info <- cycles_df %>% filter(cycle == cycle_num)
  form_betas <- estimate_betas(returns_df, market_df, cycle_info$form_start, cycle_info$form_end)
  portfolio_assignments <- form_portfolios(form_betas)
  est_results <- calc_portfolio_stats(portfolio_assignments, returns_df, market_df, cycle_info$est_start, cycle_info$est_end)
  indiv_betas_current <- est_results$individual_stats
  est_start_year <- year(as.Date(cycle_info$est_start))
  test_months <- seq(cycle_info$test_start, cycle_info$test_end, by = 1 / 12)
  results_list <- list()
  for (i in seq_along(test_months)) {
    test_month <- test_months[i]
    monthly_update <- update_betas_monthly(portfolio_assignments, indiv_betas_current, returns_df, market_df, test_month, est_start_year)
    port_stats_monthly <- monthly_update$monthly_stats
    indiv_betas_current <- monthly_update$updated_individual_betas
    port_returns <- returns_df %>%
      filter(month == test_month) %>%
      inner_join(portfolio_assignments, by = "permno") %>%
      group_by(portfolio) %>%
      summarise(ret_p = mean(ret_adj, na.rm = TRUE), .groups = "drop")
    reg_data <- port_returns %>%
      inner_join(port_stats_monthly, by = "portfolio") %>%
      filter(!is.na(ret_p) & !is.na(beta_p))
    if (nrow(reg_data) < 10) next
    tryCatch({
      model_a <- lm(ret_p ~ beta_p, data = reg_data)
      model_b <- lm(ret_p ~ beta_p + beta_sq_p, data = reg_data)
      model_c <- lm(ret_p ~ beta_p + resid_sd_p, data = reg_data)
      model_d <- lm(ret_p ~ beta_p + beta_sq_p + resid_sd_p, data = reg_data)
      results_list[[length(results_list) + 1]] <- tibble(
        month = test_month,
        gamma0_a = coef(model_a)[1], gamma1_a = coef(model_a)[2], r2_a = summary(model_a)$adj.r.squared,
        gamma0_b = coef(model_b)[1], gamma1_b = coef(model_b)[2], gamma2_b = coef(model_b)[3], r2_b = summary(model_b)$adj.r.squared,
        gamma0_c = coef(model_c)[1], gamma1_c = coef(model_c)[2], gamma3_c = coef(model_c)[3], r2_c = summary(model_c)$adj.r.squared,
        gamma0_d = coef(model_d)[1], gamma1_d = coef(model_d)[2], gamma2_d = coef(model_d)[3], gamma3_d = coef(model_d)[4], r2_d = summary(model_d)$adj.r.squared
      )
    }, error = function(e) {})
  }
  return(bind_rows(results_list))
}


# --- Step 4: Main Execution ---
all_fm_results <- map_dfr(1:nrow(cycles), ~run_fama_macbeth(.x, cycles, crsp_clean, mkt_rf))


# --- Step 5: ✅ REVISED - Final Statistics Calculation ---
calculate_final_stats <- function(df, rf_df) {
  df_with_rf <- df %>% left_join(rf_df %>% select(month, rf_m), by = "month")
  
  get_stats <- function(gamma_vec, rf_vec = NULL, type = "mean") {
    if (all(is.na(gamma_vec))) return(tibble(mean=NA, sd=NA, t_stat=NA, t_stat_minus_rf=NA, rho=NA))
    valid_indices <- !is.na(gamma_vec)
    valid_vec <- gamma_vec[valid_indices]
    n_valid <- length(valid_vec)
    if (n_valid < 2) return(tibble(mean=mean(valid_vec), sd=NA, t_stat=NA, t_stat_minus_rf=NA, rho=NA))
    m <- mean(valid_vec)
    s <- sd(valid_vec)
    t <- m / (s / sqrt(n_valid))
    rho <- if (type == "mean") cor(valid_vec[-n_valid], valid_vec[-1]) else sum(valid_vec[-n_valid] * valid_vec[-1], na.rm=T) / sum(valid_vec^2, na.rm=T)
    t_minus_rf <- if (!is.null(rf_vec)) {
      valid_rf <- rf_vec[valid_indices]
      gamma_minus_rf <- valid_vec - valid_rf
      mean(gamma_minus_rf) / (sd(gamma_minus_rf) / sqrt(n_valid))
    } else { NA }
    tibble(mean = m, sd = s, t_stat = t, t_stat_minus_rf = t_minus_rf, rho = rho)
  }
  
  list(
    A = list(g0 = get_stats(df_with_rf$gamma0_a, df_with_rf$rf_m, "mean"), g1 = get_stats(df_with_rf$gamma1_a, type="mean"), r2 = tibble(mean = mean(df$r2_a, na.rm=T), sd = sd(df$r2_a, na.rm=T))),
    B = list(g0 = get_stats(df_with_rf$gamma0_b, df_with_rf$rf_m, "mean"), g1 = get_stats(df_with_rf$gamma1_b, type="mean"), g2 = get_stats(df_with_rf$gamma2_b, type="zero"), r2 = tibble(mean = mean(df$r2_b, na.rm=T), sd = sd(df$r2_b, na.rm=T))),
    C = list(g0 = get_stats(df_with_rf$gamma0_c, df_with_rf$rf_m, "mean"), g1 = get_stats(df_with_rf$gamma1_c, type="mean"), g3 = get_stats(df_with_rf$gamma3_c, type="zero"), r2 = tibble(mean = mean(df$r2_c, na.rm=T), sd = sd(df$r2_c, na.rm=T))),
    D = list(g0 = get_stats(df_with_rf$gamma0_d, df_with_rf$rf_m, "mean"), g1 = get_stats(df_with_rf$gamma1_d, type="mean"), g2 = get_stats(df_with_rf$gamma2_d, type="zero"), g3 = get_stats(df_with_rf$gamma3_d, type="zero"), r2 = tibble(mean = mean(df$r2_d, na.rm=T), sd = sd(df$r2_d, na.rm=T)))
  )
}


# --- Step 6: ✅ REVISED - Generate and Export Excel File with Exact Columns ---
reporting_periods <- tribble(
  ~period_name,   ~start,      ~end,
  "1935-6/68",    "1935-01",   "1968-06", "1935-45",      "1935-01",   "1945-12",
  "1946-55",      "1946-01",   "1955-12", "1956-6/68",    "1956-01",   "1968-06",
  "1935-40",      "1935-01",   "1940-12", "1941-45",      "1941-01",   "1945-12",
  "1946-50",      "1946-01",   "1950-12", "1951-55",      "1951-01",   "1955-12",
  "1956-60",      "1956-01",   "1960-12", "1961-6/68",    "1961-01",   "1968-06"
) %>% mutate(across(start:end, as.yearmon))

all_period_stats <- map(1:nrow(reporting_periods), ~{
  period_info <- reporting_periods[.x,]
  period_data <- all_fm_results %>% filter(month >= period_info$start & month <= period_info$end)
  calculate_final_stats(period_data, mkt_rf)
})
names(all_period_stats) <- reporting_periods$period_name

wb <- createWorkbook()
addWorksheet(wb, "Table 3 Replication")
header_style <- createStyle(textDecoration = "bold")
num_style_4d <- createStyle(numFmt = "0.0000")
num_style_2d <- createStyle(numFmt = "0.00")
current_row <- 1

for (panel in c("A", "B", "C", "D")) {
  writeData(wb, 1, paste("Panel", panel), startCol = 1, startRow = current_row)
  addStyle(wb, 1, createStyle(textDecoration="bold", fontSize=14), rows=current_row, cols=1, stack=TRUE)
  current_row <- current_row + 1
  
  # Build a comprehensive data frame with all possible columns
  panel_df <- map_dfr(reporting_periods$period_name, ~{
    stats <- all_period_stats[[.x]][[panel]]
    tibble(
      PERIOD = .x,
      `γ̄₀` = stats$g0$mean, `s(γ₀)` = stats$g0$sd, `t(γ̄₀)` = stats$g0$t_stat, `t(γ̄₀-R_f)` = stats$g0$t_stat_minus_rf,
      `γ̄₁` = stats$g1$mean, `s(γ₁)` = stats$g1$sd, `t(γ̄₁)` = stats$g1$t_stat,
      `γ̄₂` = if("g2" %in% names(stats)) stats$g2$mean else NA, `s(γ₂)` = if("g2" %in% names(stats)) stats$g2$sd else NA, `t(γ̄₂)` = if("g2" %in% names(stats)) stats$g2$t_stat else NA,
      `γ̄₃` = if("g3" %in% names(stats)) stats$g3$mean else NA, `s(γ₃)` = if("g3" %in% names(stats)) stats$g3$sd else NA, `t(γ̄₃)` = if("g3" %in% names(stats)) stats$g3$t_stat else NA,
      `ρₘ(γ₀)` = stats$g0$rho, `ρₘ(γ₁)` = stats$g1$rho,
      `ρ₀(γ₂)` = if("g2" %in% names(stats)) stats$g2$rho else NA,
      `ρ₀(γ₃)` = if("g3" %in% names(stats)) stats$g3$rho else NA,
      `r̄²` = stats$r2$mean, `s(r²)` = stats$r2$sd
    )
  })
  
  # Select and order columns exactly as they appear in the paper for each panel
  final_cols <- switch(panel,
                       "A" = c("PERIOD", "γ̄₀", "s(γ₀)", "t(γ̄₀)", "t(γ̄₀-R_f)", "γ̄₁", "s(γ₁)", "t(γ̄₁)", "ρₘ(γ₀)", "ρₘ(γ₁)", "r̄²", "s(r²)"),
                       "B" = c("PERIOD", "γ̄₀", "s(γ₀)", "t(γ̄₀)", "t(γ̄₀-R_f)", "γ̄₁", "s(γ₁)", "t(γ̄₁)", "γ̄₂", "s(γ₂)", "t(γ̄₂)", "ρₘ(γ₀)", "ρₘ(γ₁)", "ρ₀(γ₂)", "r̄²", "s(r²)"),
                       "C" = c("PERIOD", "γ̄₀", "s(γ₀)", "t(γ̄₀)", "t(γ̄₀-R_f)", "γ̄₁", "s(γ₁)", "t(γ̄₁)", "γ̄₃", "s(γ₃)", "t(γ̄₃)", "ρₘ(γ₀)", "ρₘ(γ₁)", "ρ₀(γ₃)", "r̄²", "s(r²)"),
                       "D" = c("PERIOD", "γ̄₀", "s(γ₀)", "t(γ̄₀)", "t(γ̄₀-R_f)", "γ̄₁", "s(γ₁)", "t(γ̄₁)", "γ̄₂", "s(γ₂)", "t(γ̄₂)", "γ̄₃", "s(γ₃)", "t(γ̄₃)", "ρₘ(γ₀)", "ρₘ(γ₁)", "ρ₀(γ₂)", "ρ₀(γ₃)", "r̄²", "s(r²)")
  )
  
  panel_df_final <- panel_df %>% select(all_of(final_cols))
  
  writeData(wb, 1, panel_df_final, startRow = current_row, headerStyle = header_style)
  
  # Apply formatting
  addStyle(wb, 1, num_style_4d, rows = (current_row+1):(current_row + nrow(panel_df_final)), cols = which(grepl("γ̄|s\\(|r̄²|s\\(r²\\)", colnames(panel_df_final))), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, 1, num_style_2d, rows = (current_row+1):(current_row + nrow(panel_df_final)), cols = which(grepl("t\\(γ̄|ρ", colnames(panel_df_final))), gridExpand = TRUE, stack = TRUE)
  
  # Apply borders
  addStyle(wb, 1, createStyle(border = "Top", borderStyle = "thick"), rows = current_row, cols = 1:ncol(panel_df_final), stack=TRUE)
  addStyle(wb, 1, createStyle(border = "Bottom"), rows = current_row, cols = 1:ncol(panel_df_final), stack=TRUE)
  addStyle(wb, 1, createStyle(border = "Bottom", borderStyle = "thick"), rows = current_row + nrow(panel_df_final), cols = 1:ncol(panel_df_final), stack=TRUE)
  
  current_row <- current_row + nrow(panel_df_final) + 3
}

setColWidths(wb, 1, cols = 1:20, widths = "auto")
setColWidths(wb, 1, cols = 1, widths = 12)
saveWorkbook(wb, "Fama_MacBeth_Table3_Replication.xlsx", overwrite = TRUE)

cat("\n\n✅ Successfully exported formatted results to 'Fama_MacBeth_Table3_Replication.xlsx'\n")