#======================================================
# Fama & MacBeth (1973) - TABLE 3 EXACT REPLICATION

#======================================================

library(dplyr)
library(tidyr)
library(zoo)
library(broom)
library(purrr)

# --- Initial Data Setup ---
# We start with two data frames created in the previous data extraction script:
# 1. `crsp_clean`: Contains the monthly stock returns (`ret_adj`).
#    - This `ret_adj` column already handles delisting ambiguity by compounding the
#      final return with CRSP's delisting return.
#    - The data was also filtered for ordinary common shares (CRSP shrcd 10 or 11),
#      which is how we tackle the ambiguity of what "common stock" means.
# 2. `mkt_rf`: Contains the equal-weighted market return and the risk-free rate.

# --- Market data with risk-free rate ---
# Getting market data ready for joining.
mkt_by_month <- mkt_rf %>%
  transmute(month = as.yearmon(date), Rm = mkt_eqw_ret, Rf = rf_m)

# --- Stock-month panel ---
# Combine our individual stock returns with the market data for each month.
# This gives us a single, tidy data frame to work with.
stock_m <- crsp_clean %>%
  select(permno, month, Ri = ret_adj) %>%
  inner_join(mkt_by_month, by = "month")

#======================================================
# Helper Functions
#======================================================

# This function helps us  for calculating beta. For any given stock,
# it runs a simple market model regression (stock return ~ market return).
# It also calculates `s_eps_i`, the standard deviation of the residuals, which
# we'll use as our measure of non-beta risk.
fit_mm_security <- function(df) {
  # We need at least a year of data to get a meaningful beta.
  if (nrow(df) < 12 || any(!is.finite(df$Ri)) || any(!is.finite(df$Rm))) {
    return(tibble(beta_i = NA_real_, s_eps_i = NA_real_))
  }
  fit <- lm(Ri ~ Rm, data = df)
  tibble(
    beta_i  = unname(coef(fit)[["Rm"]]), # This is the stock's beta
    s_eps_i = sd(residuals(fit))         # This is our non-beta risk measure
  )
}

# Fama & MacBeth formed 20 portfolios. This function helps us divide N stocks
# into 20 groups as evenly as possible.
# The below code distributes the residual stocks the same way suggested by the paper,by distrbuting them
# with first and last portoflios.If the total number of securities is odd, 
#the final portfolio (Portfolio 20) receives one extra security.
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

# A simple helper to create a sequence of months.
ym_seq <- function(y1, y2, m1 = 1, m2 = 12) {
  seq(as.yearmon(paste0(y1, "-", sprintf("%02d", m1))),
      as.yearmon(paste0(y2, "-", sprintf("%02d", m2))),
      by = 1/12)
}

#======================================================
# Core Function: Process one triplet (Formation, Estimation, Testing)
#======================================================

# This is the main section of the entire replication. It takes the start and end dates
# for the three main periods and performs the full Fama-MacBeth procedure.
process_triplet <- function(formation_start, formation_end,
                            est_start, est_end,
                            test_start, test_end,
                            test_end_month = 12) {
  
  # Define the three time periods for this run.
  ym_form <- ym_seq(formation_start, formation_end) # Period to form portfolios
  ym_est  <- ym_seq(est_start, est_end)           # Period to estimate portfolio risk
  ym_test <- ym_seq(test_start, test_end, m2 = test_end_month) # Period to run the main test
  
  # Figure out which stocks we can actually use. They need to have enough data
  # across all the periods to be included.
  # The filtering critierias are given below.
  # 1. Must have a valid return in the first month of the testing as stated in the paper.
  # 2. Unbroken return history for all 5 years  of the estimation period.
  # 3. Must have atleast 4 years of data in the initial portoflio formation period.
  universe <- stock_m %>%
    mutate(in_form = month %in% ym_form,
           in_est  = month %in% ym_est) %>%
    group_by(permno) %>%
    summarise(
      has_test1 = any(month == ym_test[1] & is.finite(Ri)),
      n_est     = sum(in_est  & is.finite(Ri)),
      n_form    = sum(in_form & is.finite(Ri)),
      .groups = "drop"
    ) %>%
    filter(has_test1, n_est == length(ym_est), n_form >= 48) %>%
    pull(permno)
  
  if (length(universe) < 20) {
    warning("Too few securities")
    return(NULL)
  }
  #======================================================================================  
  ## STEP 1: FORMING THE PORTFOLIOS ##
  # First, we calculate betas for all eligible stocks using the 'formation period' data.
  # Then we rank them from lowest to highest beta and group them into 20 portfolios.
  #=====================================================================================
  betas_form <- stock_m %>%
    filter(permno %in% universe, month %in% ym_form) %>%
    group_by(permno) %>%
    group_modify(~ fit_mm_security(.x)) %>%
    ungroup() %>%
    filter(is.finite(beta_i)) %>%
    arrange(beta_i) # Here's the ranking by beta
  
  # Now, we assign each stock to one of the 20 portfolios.
  sizes <- sizes_for_20(nrow(betas_form))
  membership <- betas_form %>%
    mutate(port = rep(1:20, times = sizes)) %>%
    select(permno, port)
  #=========================================================================
  ## STEP 2: ESTIMATING PORTFOLIO RISK ##
  # To avoid statistical bias, we use a totally separate 'estimation period'
  # to get cleaner estimates of risk for the 20 portfolios we just made.
  #=========================================================================
  sec_est_initial <- stock_m %>%
    filter(permno %in% membership$permno, month %in% ym_est) %>%
    group_by(permno) %>%
    group_modify(~ fit_mm_security(.x)) %>%
    ungroup() %>%
    inner_join(membership, by = "permno") %>%
    filter(is.finite(beta_i))
  
  # This list will store the results from our monthly regressions.
  results <- list()
  sec_est_current <- sec_est_initial
  
  #===================================================================================
  ## STEP 3: RUNNING THE MONTHLY REGRESSIONS ##
  # Now for the main analysis. We loop through every single month in the 'testing period'
  # and run a cross-sectional regression.
  #=====================================================================================
  for (t_idx in seq_along(ym_test)) {
    t_month <- ym_test[t_idx]
    
    # F&M updated their beta estimates each year, hence we follow the same.
    if (month(as.Date(t_month)) == 1 || t_idx == 1) {
      update_end <- if(t_idx == 1) ym_est[length(ym_est)] else t_month - 1/12
      ym_update <- seq(ym_est[1], update_end, by = 1/12)
      
      sec_est_current <- stock_m %>%
        filter(permno %in% membership$permno, month %in% ym_update) %>%
        group_by(permno) %>%
        filter(n() >= 24) %>%
        group_modify(~ fit_mm_security(.x)) %>%
        ungroup() %>%
        inner_join(membership, by = "permno") %>%
        filter(is.finite(beta_i))
    }
    
    # Get the stocks that actually traded in the current month.
    active_permnos <- stock_m %>%
      filter(month == t_month, is.finite(Ri)) %>%
      pull(permno)
    
    # Now, we calculate the risk characteristics for each of our 20 portfolios.
    # This is where we get the final portfolio beta and other risk measures for this month's test.
    port_chars <- sec_est_current %>%
      filter(permno %in% active_permnos) %>%
      group_by(port) %>%
      summarise(
        beta_p      = mean(beta_i, na.rm = TRUE),
        # Here's where we handle the non-linearity term. As planned, we first
        # square the betas of the individual stocks (`beta_i^2`), and only then do we
        # average them up to the portfolio level.
        beta_sq_p   = mean(beta_i^2, na.rm = TRUE),
        s_eps_p     = mean(s_eps_i, na.rm = TRUE),
        n_sec       = n(),
        .groups = "drop"
      ) %>%
      filter(n_sec >= 1)
    
    # Calculate the actual return for each portfolio this month (equal-weighted).
    port_rets <- stock_m %>%
      filter(month == t_month, permno %in% membership$permno, is.finite(Ri)) %>%
      inner_join(membership, by = "permno") %>%
      group_by(port) %>%
      summarise(Rp = mean(Ri, na.rm = TRUE), .groups = "drop")
    
    # Merge the returns and risk characteristics. This gives us the 20 data points
    # we need for this month's cross-sectional regression.
    reg_data <- port_rets %>%
      inner_join(port_chars, by = "port") %>%
      filter(is.finite(Rp), is.finite(beta_p))
    
    if (nrow(reg_data) < 10) next
    
    # Get the risk-free rate for this specific month.
    Rf_t <- mkt_by_month %>% filter(month == t_month) %>% pull(Rf)
    if (length(Rf_t) == 0) Rf_t <- NA_real_
    
    # Finally, run the four regression models specified in the paper.
    # The coefficients from these models are our 'gammas'.
    tryCatch({
      mod_a <- lm(Rp ~ beta_p, data = reg_data)
      mod_b <- lm(Rp ~ beta_p + beta_sq_p, data = reg_data)
      mod_c <- lm(Rp ~ beta_p + s_eps_p, data = reg_data)
      mod_d <- lm(Rp ~ beta_p + beta_sq_p + s_eps_p, data = reg_data)
      
      # We save the gamma coefficients from this month's regression.
      # After this loop finishes, we'll have a time series of these gammas.
      results[[length(results) + 1]] <- tibble(
        month = t_month,
        Rf    = Rf_t,
        g0_a = coef(mod_a)[1], g1_a = coef(mod_a)[2],
        r2_a = summary(mod_a)$adj.r.squared,
        g0_b = coef(mod_b)[1], g1_b = coef(mod_b)[2], g2_b = coef(mod_b)[3],
        r2_b = summary(mod_b)$adj.r.squared,
        g0_c = coef(mod_c)[1], g1_c = coef(mod_c)[2], g3_c = coef(mod_c)[3],
        r2_c = summary(mod_c)$adj.r.squared,
        g0_d = coef(mod_d)[1], g1_d = coef(mod_d)[2],
        g2_d = coef(mod_d)[3], g3_d = coef(mod_d)[4],
        r2_d = summary(mod_d)$adj.r.squared
      )
    }, error = function(e) NULL)
  }
  
  bind_rows(results)
}

#======================================================
# Run all 9 triplets
#======================================================

# The paper repeats the entire process 9 times on overlapping periods to
# make sure the results are robust. We define those 9 periods here.
triplets <- tibble(
  period = 1:9,
  form_start = c(1926, 1927, 1931, 1935, 1939, 1943, 1947, 1951, 1955),
  form_end   = c(1929, 1933, 1937, 1941, 1945, 1949, 1953, 1957, 1961),
  est_start  = c(1930, 1934, 1938, 1942, 1946, 1950, 1954, 1958, 1962),
  est_end    = c(1934, 1938, 1942, 1946, 1950, 1954, 1958, 1962, 1966),
  test_start = c(1935, 1939, 1943, 1947, 1951, 1955, 1959, 1963, 1967),
  test_end   = c(1938, 1942, 1946, 1950, 1954, 1958, 1962, 1966, 1968),
  test_end_m = c(  12,   12,   12,   12,   12,   12,   12,   12,    6)
)

cat("Running Fama-MacBeth regressions...\n")

# This is where we run the core function for each of the 9 periods.
all_results <- triplets %>%
  rowwise() %>%
  mutate(
    results = list(process_triplet(form_start, form_end,
                                   est_start, est_end,
                                   test_start, test_end, test_end_m))
  ) %>%
  pull(results) %>%
  bind_rows()

cat("Completed", nrow(all_results), "monthly regressions\n\n")

#======================================================
# Calculate ALL statistics for Table 3
#======================================================
calc_table3_stats <- function(df, model_suffix) {
  
  g0_col <- paste0("g0_", model_suffix)
  g1_col <- paste0("g1_", model_suffix)
  g2_col <- paste0("g2_", model_suffix)
  g3_col <- paste0("g3_", model_suffix)
  r2_col <- paste0("r2_", model_suffix)
  
  n <- nrow(df)
  
  # Function to calculate all stats for one variable
  calc_all_stats <- function(vals, var_name, include_rf_test = FALSE) {
    vals_clean <- vals[is.finite(vals)]
    n_valid <- length(vals_clean)
    
    if (n_valid == 0) {
      result <- tibble(
        Variable = var_name,
        mean = NA, sd = NA, t_stat = NA,
        rho_m = NA, rho_0 = NA
      )
      if (include_rf_test) {
        result$t_g0_minus_rf <- NA
      }
      return(result)
    }
    
    mn <- mean(vals_clean)
    s <- sd(vals_clean)
    t_stat <- mn / (s / sqrt(n_valid))
    
    # Serial correlations
    if (n_valid > 1) {
      rho_m <- cor(vals_clean[-n_valid] - mn, vals_clean[-1] - mn, 
                   use = "complete.obs")
      rho_0 <- cor(vals_clean[-n_valid], vals_clean[-1], 
                   use = "complete.obs")
    } else {
      rho_m <- NA
      rho_0 <- NA
    }
    
    result <- tibble(
      Variable = var_name,
      mean = mn,
      sd = s,
      t_stat = t_stat,
      rho_m = rho_m,
      rho_0 = rho_0
    )
    
    # Add t(γ̂₀ - Rf) test for gamma 0
    if (include_rf_test && "Rf" %in% names(df)) {
      g0_clean <- vals_clean
      rf_clean <- df$Rf[is.finite(df[[g0_col]])]
      diff <- g0_clean - rf_clean
      if (length(diff) > 0) {
        mn_diff <- mean(diff, na.rm = TRUE)
        sd_diff <- sd(diff, na.rm = TRUE)
        result$t_g0_minus_rf <- mn_diff / (sd_diff / sqrt(length(diff)))
      } else {
        result$t_g0_minus_rf <- NA
      }
    } else if (include_rf_test) {
      result$t_g0_minus_rf <- NA
    }
    
    return(result)
  }
  
  # Gamma 0 - with RF test
  stats_g0 <- calc_all_stats(df[[g0_col]], "γ̂₀", include_rf_test = TRUE)
  
  # Gamma 1
  stats_g1 <- calc_all_stats(df[[g1_col]], "γ̂₁")
  stats_g1$t_g0_minus_rf <- NA  # Add column for consistency
  
  # Gamma 2 (if exists)
  stats_g2 <- NULL
  if (g2_col %in% names(df)) {
    stats_g2 <- calc_all_stats(df[[g2_col]], "γ̂₂")
    stats_g2$t_g0_minus_rf <- NA  # Add column for consistency
  }
  
  # Gamma 3 (if exists)
  stats_g3 <- NULL
  if (g3_col %in% names(df)) {
    stats_g3 <- calc_all_stats(df[[g3_col]], "γ̂₃")
    stats_g3$t_g0_minus_rf <- NA  # Add column for consistency
  }
  
  # R-squared
  r2_vals <- df[[r2_col]][is.finite(df[[r2_col]])]
  if (length(r2_vals) > 0) {
    stats_r2 <- tibble(
      Variable = "r̄²",
      mean = mean(r2_vals),
      sd = sd(r2_vals),
      t_stat = NA,
      rho_m = NA,
      rho_0 = NA,
      t_g0_minus_rf = NA
    )
  } else {
    stats_r2 <- NULL
  }
  
  # Combine all
  bind_rows(stats_g0, stats_g1, stats_g2, stats_g3, stats_r2)
}
#======================================================
# Define all 10 periods for Table 3
#======================================================

periods_table3 <- tribble(
  ~label,       ~start, ~start_m, ~end,  ~end_m,
  "1935-6/68",   1935,    1,      1968,    6,
  "1935-45",     1935,    1,      1945,   12,
  "1946-55",     1946,    1,      1955,   12,
  "1956-6/68",   1956,    1,      1968,    6,
  "1935-40",     1935,    1,      1940,   12,
  "1941-45",     1941,    1,      1945,   12,
  "1946-50",     1946,    1,      1950,   12,
  "1951-55",     1951,    1,      1955,   12,
  "1956-60",     1956,    1,      1960,   12,
  "1961-6/68",   1961,    1,      1968,    6
) %>%
  mutate(
    start_ym = as.yearmon(paste0(start, "-", sprintf("%02d", start_m))),
    end_ym   = as.yearmon(paste0(end, "-", sprintf("%02d", end_m)))
  )

#======================================================
# Generate Table 3 statistics for all periods and models
#======================================================

table3_list <- list()

for (i in 1:nrow(periods_table3)) {
  p <- periods_table3[i, ]
  
  df_period <- all_results %>%
    filter(month >= p$start_ym, month <= p$end_ym)
  
  if (nrow(df_period) == 0) next
  
  # Calculate stats for all 4 models
  stats_a <- calc_table3_stats(df_period, "a") %>% mutate(Panel = "A")
  stats_b <- calc_table3_stats(df_period, "b") %>% mutate(Panel = "B")
  stats_c <- calc_table3_stats(df_period, "c") %>% mutate(Panel = "C")
  stats_d <- calc_table3_stats(df_period, "d") %>% mutate(Panel = "D")
  
  combined <- bind_rows(stats_a, stats_b, stats_c, stats_d) %>%
    mutate(Period = p$label) %>%
    select(Period, Panel, everything())
  
  table3_list[[p$label]] <- combined
}

table3_full <- bind_rows(table3_list)


#======================================================
# CREATE EXACT TABLE 3 FORMAT (21 columns per period)
#======================================================

create_table3_exact <- function(table3_full) {
  
  # Split into two period groups
  periods_1_5 <- c("1935-6/68", "1935-45", "1946-55", "1956-6/68", "1935-40")
  periods_6_10 <- c("1941-45", "1946-50", "1951-55", "1956-60", "1961-6/68")
  
  results <- list()
  
  for (panel_letter in c("A", "B", "C", "D")) {
    
    panel_data <- table3_full %>% 
      filter(Panel == panel_letter)
    
    # Periods 1-5
    data_1_5 <- panel_data %>%
      filter(Period %in% periods_1_5) %>%
      mutate(Period = factor(Period, levels = periods_1_5)) %>%
      arrange(Period, Variable)
    
    # Check if t_g0_minus_rf exists, if not add it
    if (!"t_g0_minus_rf" %in% names(data_1_5)) {
      data_1_5 <- data_1_5 %>% mutate(t_g0_minus_rf = NA_real_)
    }
    
    data_1_5 <- data_1_5 %>%
      select(Period, Variable, mean, sd, t_stat, rho_m, rho_0, t_g0_minus_rf)
    
    # Periods 6-10
    data_6_10 <- panel_data %>%
      filter(Period %in% periods_6_10) %>%
      mutate(Period = factor(Period, levels = periods_6_10)) %>%
      arrange(Period, Variable)
    
    # Check if t_g0_minus_rf exists, if not add it
    if (!"t_g0_minus_rf" %in% names(data_6_10)) {
      data_6_10 <- data_6_10 %>% mutate(t_g0_minus_rf = NA_real_)
    }
    
    data_6_10 <- data_6_10 %>%
      select(Period, Variable, mean, sd, t_stat, rho_m, rho_0, t_g0_minus_rf)
    
    results[[paste0("Panel_", panel_letter, "_Periods_1_5")]] <- data_1_5
    results[[paste0("Panel_", panel_letter, "_Periods_6_10")]] <- data_6_10
  }
  
  results
}

table3_formatted <- create_table3_exact(table3_full)

#======================================================
# PRINT TABLE 3 IN EXACT PAPER FORMAT
#======================================================

print_table3_paper <- function(table3_formatted) {
  
  panel_titles <- c(
    "A" = "PANEL A: Rₚₜ = γ₀ₜ + γ₁ₜβₚ + ηₚₜ",
    "B" = "PANEL B: Rₚₜ = γ₀ₜ + γ₁ₜβₚ + γ₂ₜβₚ² + ηₚₜ",
    "C" = "PANEL C: Rₚₜ = γ₀ₜ + γ₁ₜβₚ + γ₃ₜs̄ₚ(εᵢ) + ηₚₜ",
    "D" = "PANEL D: Rₚₜ = γ₀ₜ + γ₁ₜβₚ + γ₂ₜβₚ² + γ₃ₜs̄ₚ(εᵢ) + ηₚₜ"
  )
  
  cat("\n")
  cat(rep("=", 180), "\n", sep = "")
  cat("TABLE 3\n")
  cat("Risk, Return, and Equilibrium: Empirical Tests\n")
  cat(rep("=", 180), "\n\n", sep = "")
  
  for (panel_letter in c("A", "B", "C", "D")) {
    
    cat("\n", rep("-", 180), "\n", sep = "")
    cat(panel_titles[panel_letter], "\n")
    cat(rep("-", 180), "\n\n", sep = "")
    
    # PERIODS 1-5
    cat("PERIODS\n")
    cat(sprintf("%-15s", " "))
    for (i in 1:5) {
      cat(sprintf("%20s", i))
    }
    cat("\n")
    cat(sprintf("%-15s", " "))
    periods_1_5 <- c("1935-6/68", "1935-45", "1946-55", "1956-6/68", "1935-40")
    for (p in periods_1_5) {
      cat(sprintf("%20s", p))
    }
    cat("\n\n")
    
    data_1_5 <- table3_formatted[[paste0("Panel_", panel_letter, "_Periods_1_5")]]
    
    if (!is.null(data_1_5) && nrow(data_1_5) > 0) {
      
      # Get unique periods and variables
      periods <- levels(data_1_5$Period)
      variables <- unique(data_1_5$Variable)
      
      # Print each statistic row
      for (var in variables) {
        
        # Mean row
        cat(sprintf("%-15s", var))
        for (p in periods) {
          val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(mean)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.4f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # SD row
        cat(sprintf("%-15s", "s(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(sd)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.4f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # t-stat row
        cat(sprintf("%-15s", "t(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(t_stat)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.2f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # rho_m row
        cat(sprintf("%-15s", "ρ̂ₘ(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(rho_m)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.3f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # rho_0 row
        cat(sprintf("%-15s", "ρ̂₀(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(rho_0)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.3f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # t(γ̂₀ - Rf) row for γ̂₀ only
        if (var == "γ̂₀") {
          cat(sprintf("%-15s", "t(γ̂₀-Rf)"))
          for (p in periods) {
            val <- data_1_5 %>% filter(Period == p, Variable == var) %>% pull(t_g0_minus_rf)
            if (length(val) > 0 && is.finite(val)) {
              cat(sprintf("%20.2f", val))
            } else {
              cat(sprintf("%20s", ""))
            }
          }
          cat("\n")
        }
        
        cat("\n")
      }
    }
    
    cat("\n")
    
    # PERIODS 6-10
    cat("PERIODS\n")
    cat(sprintf("%-15s", " "))
    for (i in 6:10) {
      cat(sprintf("%20s", i))
    }
    cat("\n")
    cat(sprintf("%-15s", " "))
    periods_6_10 <- c("1941-45", "1946-50", "1951-55", "1956-60", "1961-6/68")
    for (p in periods_6_10) {
      cat(sprintf("%20s", p))
    }
    cat("\n\n")
    
    data_6_10 <- table3_formatted[[paste0("Panel_", panel_letter, "_Periods_6_10")]]
    
    if (!is.null(data_6_10) && nrow(data_6_10) > 0) {
      
      periods <- levels(data_6_10$Period)
      variables <- unique(data_6_10$Variable)
      
      for (var in variables) {
        
        # Mean row
        cat(sprintf("%-15s", var))
        for (p in periods) {
          val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(mean)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.4f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # SD row
        cat(sprintf("%-15s", "s(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(sd)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.4f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # t-stat row
        cat(sprintf("%-15s", "t(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(t_stat)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.2f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # rho_m row
        cat(sprintf("%-15s", "ρ̂ₘ(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(rho_m)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.3f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # rho_0 row
        cat(sprintf("%-15s", "ρ̂₀(γ̂ⱼ)"))
        for (p in periods) {
          val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(rho_0)
          if (length(val) > 0 && is.finite(val)) {
            cat(sprintf("%20.3f", val))
          } else {
            cat(sprintf("%20s", ""))
          }
        }
        cat("\n")
        
        # t(γ̂₀ - Rf) row for γ̂₀ only
        if (var == "γ̂₀") {
          cat(sprintf("%-15s", "t(γ̂₀-Rf)"))
          for (p in periods) {
            val <- data_6_10 %>% filter(Period == p, Variable == var) %>% pull(t_g0_minus_rf)
            if (length(val) > 0 && is.finite(val)) {
              cat(sprintf("%20.2f", val))
            } else {
              cat(sprintf("%20s", ""))
            }
          }
          cat("\n")
        }
        
        cat("\n")
      }
    }
    
    cat("\n\n")
  }
}

# Print the table
print_table3_paper(table3_formatted)


#======================================================
# Export to Excel with proper formatting 
#======================================================

library(openxlsx)
library(dplyr)

export_table3_corrected <- function(table3_full) {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Table 3")
  
  current_row <- 1
  
  # Title
  writeData(wb, "Table 3", "TABLE 3", startRow = current_row, startCol = 1)
  addStyle(wb, "Table 3", 
           createStyle(fontSize = 16, textDecoration = "bold"), 
           rows = current_row, cols = 1)
  current_row <- current_row + 1
  
  # Subtitle
  writeData(wb, "Table 3", 
            "Risk, Return, and Equilibrium: Empirical Tests", 
            startRow = current_row, startCol = 1)
  current_row <- current_row + 3
  
  # CORRECTED column order as per Fama-MacBeth (1973) Table 3
  full_header <- c(
    "Period",
    "γ̂₀", "γ̂₁", "γ̂₂", "γ̂₃",
    "t(γ̂₀-Rf)",
    "s(γ̂₀)", "s(γ̂₁)", "s(γ̂₂)", "s(γ̂₃)",
    "ρ̂₀(γ̂₀-Rf)", "ρ̂ₘ(γ̂₁)", "ρ̂₀(γ̂₂)", "ρ̂₀(γ̂₃)",
    "t(γ̂₀)", "t(γ̂₁)", "t(γ̂₂)", "t(γ̂₃)",
    "t(γ̂₀-Rf)",
    "r̄²", "s(r̄²)"
  )
  
  # Write header
  writeData(wb, "Table 3", t(full_header), 
            startRow = current_row, startCol = 1, colNames = FALSE)
  
  header_style <- createStyle(textDecoration = "bold", 
                              border = "bottom", fgFill = "#F0F0F0")
  addStyle(wb, "Table 3", header_style, 
           rows = current_row, cols = 1:length(full_header), gridExpand = TRUE)
  
  current_row <- current_row + 2
  
  # Period order
  periods_ordered <- c("1935-6/68", "1935-45", "1946-55", "1956-6/68", "1935-40",
                       "1941-45", "1946-50", "1951-55", "1956-60", "1961-6/68")
  
  panel_titles <- c("A" = "PANEL A", "B" = "PANEL B", 
                    "C" = "PANEL C", "D" = "PANEL D")
  
  fmt <- function(x) {
    if (is.na(x)) return(NA)
    round(x, 4)
  }
  
  # Process each panel
  for (panel_letter in c("A", "B", "C", "D")) {
    
    panel_data <- table3_full %>% filter(Panel == panel_letter)
    
    # Ensure t_g0_minus_rf column exists
    if (!"t_g0_minus_rf" %in% names(panel_data)) {
      panel_data <- panel_data %>% mutate(t_g0_minus_rf = NA_real_)
    }
    
    # Write panel title
    writeData(wb, "Table 3", panel_titles[panel_letter], 
              startRow = current_row, startCol = 1)
    addStyle(wb, "Table 3", 
             createStyle(fontSize = 11, textDecoration = "bold"), 
             rows = current_row, cols = 1)
    current_row <- current_row + 1
    
    # Process each period
    for (period in periods_ordered) {
      row_data <- rep(NA, length(full_header))
      row_data[1] <- period
      
      # Extract data for each variable
      g0 <- panel_data %>% filter(Variable == "γ̂₀", Period == period)
      g1 <- panel_data %>% filter(Variable == "γ̂₁", Period == period)
      g2 <- panel_data %>% filter(Variable == "γ̂₂", Period == period)
      g3 <- panel_data %>% filter(Variable == "γ̂₃", Period == period)
      r2 <- panel_data %>% filter(Variable == "r̄²", Period == period)
      
      # Column 2-5: γ̂₀, γ̂₁, γ̂₂, γ̂₃ (means)
      if (nrow(g0) > 0) row_data[2] <- fmt(g0$mean[1])
      if (nrow(g1) > 0) row_data[3] <- fmt(g1$mean[1])
      if (nrow(g2) > 0) row_data[4] <- fmt(g2$mean[1])
      if (nrow(g3) > 0) row_data[5] <- fmt(g3$mean[1])
      
      # Column 6: t(γ̂₀-Rf) - FIRST occurrence
      if (nrow(g0) > 0) row_data[6] <- fmt(g0$t_g0_minus_rf[1])
      
      # Column 7-10: s(γ̂₀), s(γ̂₁), s(γ̂₂), s(γ̂₃) (standard deviations)
      if (nrow(g0) > 0) row_data[7] <- fmt(g0$sd[1])
      if (nrow(g1) > 0) row_data[8] <- fmt(g1$sd[1])
      if (nrow(g2) > 0) row_data[9] <- fmt(g2$sd[1])
      if (nrow(g3) > 0) row_data[10] <- fmt(g3$sd[1])
      
      # Column 11: ρ̂₀(γ̂₀-Rf) - This should be rho_0 for γ̂₀
      if (nrow(g0) > 0) row_data[11] <- fmt(g0$rho_0[1])
      
      # Column 12: ρ̂ₘ(γ̂₁) - This should be rho_m for γ̂₁
      if (nrow(g1) > 0) row_data[12] <- fmt(g1$rho_m[1])
      
      # Column 13-14: ρ̂₀(γ̂₂), ρ̂₀(γ̂₃) - rho_0 for γ̂₂ and γ̂₃
      if (nrow(g2) > 0) row_data[13] <- fmt(g2$rho_0[1])
      if (nrow(g3) > 0) row_data[14] <- fmt(g3$rho_0[1])
      
      # Column 15-18: t(γ̂₀), t(γ̂₁), t(γ̂₂), t(γ̂₃) (t-statistics)
      if (nrow(g0) > 0) row_data[15] <- fmt(g0$t_stat[1])
      if (nrow(g1) > 0) row_data[16] <- fmt(g1$t_stat[1])
      if (nrow(g2) > 0) row_data[17] <- fmt(g2$t_stat[1])
      if (nrow(g3) > 0) row_data[18] <- fmt(g3$t_stat[1])
      
      # Column 19: t(γ̂₀-Rf) - SECOND occurrence (duplicate of column 6)
      if (nrow(g0) > 0) row_data[19] <- fmt(g0$t_g0_minus_rf[1])
      
      # Column 20-21: r̄², s(r̄²)
      if (nrow(r2) > 0) {
        row_data[20] <- fmt(r2$mean[1])
        row_data[21] <- fmt(r2$sd[1])
      }
      
      writeData(wb, "Table 3", t(row_data), 
                startRow = current_row, startCol = 1, colNames = FALSE)
      current_row <- current_row + 1
    }
    
    current_row <- current_row + 2
  }
  
  # Auto-size columns
  setColWidths(wb, "Table 3", cols = 1:length(full_header), widths = "auto")
  
  # Format numbers with 4 decimals
  num_style_4 <- createStyle(numFmt = "0.0000")
  data_start_row <- 6
  data_end_row <- current_row - 1
  
  for (col in 2:21) {
    addStyle(wb, "Table 3", num_style_4, 
             rows = data_start_row:data_end_row, cols = col, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  # Save workbook
  saveWorkbook(wb, "Table3_FamaMacBeth_Corrected.xlsx", overwrite = TRUE)
  
  cat("\n✓ Exported Table3_FamaMacBeth_Corrected.xlsx\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Column order (21 columns total):\n")
  cat("  1.  Period\n")
  cat("  2-5.  γ̂₀, γ̂₁, γ̂₂, γ̂₃\n")
  cat("  6.  t(γ̂₀-Rf)\n")
  cat("  7-10.  s(γ̂₀), s(γ̂₁), s(γ̂₂), s(γ̂₃)\n")
  cat("  11.  ρ̂₀(γ̂₀-Rf)\n")
  cat("  12.  ρ̂ₘ(γ̂₁)\n")
  cat("  13-14.  ρ̂₀(γ̂₂), ρ̂₀(γ̂₃)\n")
  cat("  15-18.  t(γ̂₀), t(γ̂₁), t(γ̂₂), t(γ̂₃)\n")
  cat("  19.  t(γ̂₀-Rf)\n")
  cat("  20-21.  r̄², s(r̄²)\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("✓ All values formatted with 4 decimals\n\n")
}

# Run the corrected export function
export_table3_corrected(table3_full)