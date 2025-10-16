# TABLE 2 


library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(broom)

# (1) DATA ----------------------

# Market factor by month (equal-weight NYSE)
mkt_by_month <- fisher_eqw %>%
  transmute(month, Rm = mkt_eqw_ret)

# Stock–month panel: join delisting-adjusted returns with the market factor
stock_m <- crsp_clean %>%
  select(permno, month, Ri = ret_adj) %>%
  inner_join(mkt_by_month, by = "month")

# Per-security market model over a window → β_i and s(ε_i)
fit_mm_security <- function(df) {
  # df must contain Ri and Rm
  if (nrow(df) < 12 || any(!is.finite(df$Ri)) || any(!is.finite(df$Rm))) {
    return(tibble(beta_i = NA_real_, s_eps_i = NA_real_))
  }
  fit <- lm(Ri ~ Rm, data = df)
  tibble(
    beta_i  = unname(coef(fit)[["Rm"]]),
    s_eps_i = sd(residuals(fit))
  )
}

# Portfolio-level market model on a series → R^2, s(R_p), s(ε_p)
fit_mm_portfolio <- function(port_ret_est, mkt_est) {
  mm   <- lm(port_ret_est ~ mkt_est)
  r2   <- summary(mm)$r.squared
  sRp  <- sd(port_ret_est)
  s_ep <- sd(residuals(mm))
  tibble(r2 = r2, sRp = sRp, s_ep = s_ep)
}

# Sizing rule for 20 portfolios (middle 18 = floor(N/20); remainder split to 1 and 20)
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

# (2) TABLE-BUILDING --------------------

# Statistic display order used in the paper-style table
.stat_order <- c(
  "β̂_{p,t−1}",
  "s(β̂_{p,t−1})",
  "r(R_p, R_m)^2",
  "s(R_p)",
  "s(ε̂_p)",
  "s̄_{p,t−1}(ε_i)",
  "s(ε̂_p)/s̄_{p,t−1}(ε_i)"
)

# Build a half-table (10 portfolios) with: Statistic row + period banner row
make_half_table <- function(stats_tbl, idx, period_label) {
  wide <- stats_tbl %>%
    dplyr::filter(Portfolio %in% idx) %>%
    dplyr::arrange(Portfolio) %>%
    tidyr::pivot_longer(-Portfolio, names_to = "Statistic", values_to = "Value") %>%
    dplyr::mutate(Statistic = factor(Statistic, levels = .stat_order)) %>%
    dplyr::arrange(Statistic, Portfolio) %>%
    dplyr::select(Statistic, Portfolio, Value) %>%
    tidyr::pivot_wider(names_from = Portfolio, values_from = Value)
  
  # Column names consistent with the paper
  names(wide)[1]  <- " "
  names(wide)[-1] <- as.character(idx)
  wide <- wide %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # Row 1: "Statistic | 1 2 …"
  header_stat <- tibble::as_tibble_row(
    stats::setNames(as.list(c("Statistic", as.character(idx))), names(wide))
  )
  
  # Row 2: left-aligned period banner (label in the first column)
  header_period <- tibble::as_tibble_row(
    stats::setNames(as.list(c(period_label, rep("", length(idx)))), names(wide))
  )
  
  dplyr::bind_rows(header_stat, header_period, wide)
}

# Compute Table 2 statistics for one triplet (formation, estimation, first test month)
table2_for_triplet <- function(formation_start, formation_end,
                               est_start, est_end,
                               test_first_year, test_first_month = 1) {
  
  ym_form  <- seq(as.yearmon(paste0(formation_start, "-01")),
                  as.yearmon(paste0(formation_end,   "-12")), by = 1/12)
  ym_est   <- seq(as.yearmon(paste0(est_start,      "-01")),
                  as.yearmon(paste0(est_end,        "-12")), by = 1/12)
  ym_test1 <- as.yearmon(paste0(test_first_year, "-", sprintf("%02d", test_first_month)))
  
  # Universe selection: must be present at first test month; full estimation coverage; ≥48 formation months
  universe <- stock_m %>%
    dplyr::mutate(in_form = month %in% ym_form, in_est = month %in% ym_est) %>%
    dplyr::group_by(permno) %>%
    dplyr::summarise(
      has_test1 = any(month == ym_test1 & is.finite(Ri)),
      n_est     = sum(in_est  & is.finite(Ri)),
      n_form    = sum(in_form & is.finite(Ri)),
      .groups = "drop"
    ) %>%
    dplyr::filter(has_test1, n_est == length(ym_est), n_form >= 48) %>%
    dplyr::pull(permno)
  
  if (length(universe) < 20) stop("Too few securities in universe for this period.")
  
  # Sort securities by formation-window beta_i and assign 20 portfolios
  betas_form <- stock_m %>%
    dplyr::filter(permno %in% universe, month %in% ym_form) %>%
    dplyr::group_by(permno) %>%
    dplyr::group_modify(~ fit_mm_security(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(beta_i)) %>%
    dplyr::arrange(beta_i)
  
  sizes <- sizes_for_20(nrow(betas_form))
  membership <- betas_form %>%
    dplyr::mutate(port = rep(1:20, times = sizes)) %>%
    dplyr::select(permno, port)
  
  # Estimation-window per-security betas & residual s.d., merged with portfolio membership
  sec_est <- stock_m %>%
    dplyr::filter(permno %in% membership$permno, month %in% ym_est) %>%
    dplyr::group_by(permno) %>%
    dplyr::group_modify(~ fit_mm_security(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(membership, by = "permno")
  
  # Estimation-window equal-weight portfolio returns; join market; fit portfolio market model
  port_month_est <- stock_m %>%
    dplyr::filter(permno %in% membership$permno, month %in% ym_est) %>%
    dplyr::inner_join(membership, by = "permno") %>%
    dplyr::group_by(port, month) %>%
    dplyr::summarise(Rp = mean(Ri, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(port, month) %>%
    dplyr::inner_join(mkt_by_month %>% dplyr::filter(month %in% ym_est), by = "month")
  
  port_fit <- port_month_est %>%
    dplyr::group_by(port) %>%
    dplyr::summarise(fit = list(fit_mm_portfolio(Rp, Rm)), .groups = "drop") %>%
    tidyr::unnest(fit)
  
  # s(β̂_{p,t−1}) = s(ε_p) / (sqrt(T) * s(R_m))
  n_est <- length(ym_est)
  sRm   <- stock_m %>% dplyr::filter(month %in% ym_est) %>% dplyr::distinct(month, Rm) %>% dplyr::pull(Rm) %>% stats::sd()
  port_fit <- port_fit %>% dplyr::mutate(s_beta = s_ep / (sqrt(n_est) * sRm))
  
  # Portfolio averages of per-security betas and residual s.d.
  beta_bar <- sec_est %>% dplyr::group_by(port) %>% dplyr::summarise(beta_bar   = mean(beta_i,  na.rm = TRUE), .groups = "drop")
  sbar_eps <- sec_est %>% dplyr::group_by(port) %>% dplyr::summarise(sbar_eps_i = mean(s_eps_i, na.rm = TRUE), .groups = "drop")
  
  # Assemble Table 2 statistics
  stats <- beta_bar %>%
    dplyr::inner_join(port_fit, by = "port") %>%
    dplyr::inner_join(sbar_eps, by = "port") %>%
    dplyr::transmute(
      Portfolio                = port,
      `β̂_{p,t−1}`              = round(beta_bar,     3),
      `s(β̂_{p,t−1})`           = round(s_beta,       3),
      `r(R_p, R_m)^2`          = round(r2,           3),
      `s(R_p)`                 = round(sRp,          3),
      `s(ε̂_p)`                 = round(s_ep,         3),
      `s̄_{p,t−1}(ε_i)`         = round(sbar_eps_i,   3),
      `s(ε̂_p)/s̄_{p,t−1}(ε_i)` = round(s_ep/sbar_eps_i, 3)
    ) %>%
    dplyr::arrange(Portfolio)
  
  period_label <- sprintf("Portfolios for Estimation Period %d–%02d",
                          est_start, est_end %% 100)
  
  list(
    panel_1to10  = make_half_table(stats, 1:10,  period_label),
    panel_11to20 = make_half_table(stats, 11:20, period_label)
  )
}

# Build a master table WITHOUT the internal title row
# and WITHOUT the "Statistic | 1..n" number row.
make_master_table <- function(p1, p2, p3, p4) {
  panels <- list(p1, p2, p3, p4)
  
  # Harmonize column names and coerce to character
  cols <- names(panels[[1]])
  panels <- lapply(panels, function(x) {
    names(x) <- cols
    dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
  })
  
  # Helper: drop the first row (the "Statistic | 1..n" row)
  drop_numbers_row <- function(df) df[-1, , drop = FALSE]
  
  # Drop the numbers row from all four panels
  panels <- lapply(panels, drop_numbers_row)
  
  # Spacer between period blocks (blank row)
  spacer <- tibble::as_tibble_row(stats::setNames(as.list(rep("", length(cols))), cols))
  
  # Bind blocks: keep each period's banner row as the first row of each block
  dplyr::bind_rows(
    panels[[1]], spacer,
    panels[[2]], spacer,
    panels[[3]], spacer,
    panels[[4]]
  )
}

# (3) ASSEMBLY & EXPORT -----------

# Compute runs for four estimation periods (formation, estimation, first test month)
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

# Extract precomputed panels (1–10) and (11–20)
table2_1934_38_A <- runs[["1934–38"]]$panel_1to10
table2_1942_46_A <- runs[["1942–46"]]$panel_1to10
table2_1950_54_A <- runs[["1950–54"]]$panel_1to10
table2_1958_62_A <- runs[["1958–62"]]$panel_1to10

table2_1934_38_B <- runs[["1934–38"]]$panel_11to20
table2_1942_46_B <- runs[["1942–46"]]$panel_11to20
table2_1950_54_B <- runs[["1950–54"]]$panel_11to20
table2_1958_62_B <- runs[["1958–62"]]$panel_11to20

# Export Table 2
export_table2_corrected <- function(df, file = NULL,
                                    title = "SAMPLE STATISTICS FOR FOUR SELECTED ESTIMATION PERIODS") {
  if (!is.null(file)) {
    utils::write.csv(df, file, row.names = FALSE)
    message("Saved: ", file)
  }
  
  if (requireNamespace("gt", quietly = TRUE)) {
    df |>
      gt::gt() |>
      gt::tab_header(title = gt::md(paste0("**", title, "**"))) |>
      gt::cols_align(
        align = "center",
        columns = -1
      ) |>
      gt::cols_align(
        align = "left",
        columns = 1
      ) |>
      gt::fmt_missing(everything(), missing_text = "") |>
      print()
  } else {
    cat("\n", title, "\n", sep = "")
    print(as.data.frame(df), row.names = FALSE)
  }
}
# Build master tables with the numbers row removed
table2_master_1to10  <- make_master_table(
  table2_1934_38_A, table2_1942_46_A, table2_1950_54_A, table2_1958_62_A
)
table2_master_11to20 <- make_master_table(
  table2_1934_38_B, table2_1942_46_B, table2_1950_54_B, table2_1958_62_B
)

# Render with a single bold heading
export_table2_corrected(table2_master_1to10,
                        title = "Table 2. Sample Statistics For Four Selected Estimation Periods")
export_table2_corrected(table2_master_11to20,
                        title = "Table 2. (Continued)")


export_table2_corrected <- function(df, file = NULL,
                                    title = "SAMPLE STATISTICS FOR FOUR SELECTED ESTIMATION PERIODS") {
  gt_tbl <- df |>
    gt::gt() |>
    gt::tab_header(title = gt::md(paste0("**", title, "**"))) |>
    gt::cols_align(align = "center", columns = -1) |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::fmt_missing(everything(), missing_text = "")
  
  if (!is.null(file)) {
    # Automatically detect extension and export
    gt::gtsave(gt_tbl, filename = file)
    message("Exported: ", file)
  } else {
    print(gt_tbl)
  }
  
  invisible(gt_tbl)
}

# Save to PDF
export_table2_corrected(table2_master_1to10, file = "Table 2.pdf")
export_table2_corrected(table2_master_11to20, file = "Table 2_(continued).pdf")

### Export for Excel file

write_xlsx(
  list(
    "Table2_PanelA_1-10"  = table2_master_1to10,
    "Table2_PanelB_11-20" = table2_master_11to20
  ),
  path = "Table 2.xlsx"
)

