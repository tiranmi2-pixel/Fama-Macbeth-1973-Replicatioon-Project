
# Table 1 — Portfolio formation, estimation, and testing periods
fmt_span <- function(y1, y2) sprintf("%d–%02d", y1, y2 %% 100)

# ---- 0) Ensure we have a monthly key and a clean return flag
panel_m <- crsp_clean %>%
  mutate(month = as.yearmon(month),
         has_ret = is.finite(ret_adj)) %>%
  select(permno, month, has_ret)


# ---- 1) Define the 9 period triplets exactly as in Table 1
periods <- tibble::tibble(
  period = 1:9,
  form_start = c(1926, 1927, 1931, 1935, 1939, 1943, 1947, 1951, 1955),
  form_end   = c(1929, 1933, 1937, 1941, 1945, 1949, 1953, 1957, 1961),
  est_start  = c(1930, 1934, 1938, 1942, 1946, 1950, 1954, 1958, 1962),
  est_end    = c(1934, 1938, 1942, 1946, 1950, 1954, 1958, 1962, 1966),
  test_start = c(1935, 1939, 1943, 1947, 1951, 1955, 1959, 1963, 1967),
  test_end   = c(1938, 1942, 1946, 1950, 1954, 1958, 1962, 1966, 1968)
) %>%
  mutate(
    form_span = purrr::pmap_chr(list(form_start, form_end), fmt_span),
    est_span  = purrr::pmap_chr(list(est_start,  est_end),  fmt_span),
    test_span = purrr::pmap_chr(list(test_start, test_end), fmt_span)
  )


# Helper: build full yearmon sequences
ym_seq <- function(y1, y2) seq(as.yearmon(paste0(y1, "-01")), as.yearmon(paste0(y2, "-12")), by = 1/12)


# ---- 2) Function to compute the two counts for one period
count_for_period <- function(p) {
  # windows
  ym_form <- ym_seq(p$form_start, p$form_end)
  ym_est  <- ym_seq(p$est_start,  p$est_end)
  ym_test <- ym_seq(p$test_start, p$test_end)
  
  # first month of testing period
  first_test_month <- ym_test[1]
  
  # securities available in first test month
  avail <- panel_m %>%
    filter(month == first_test_month, has_ret) %>%
    distinct(permno)
  
  n_available <- nrow(avail)
  
  # coverage in estimation (require ALL 60 months)
  est_cover <- panel_m %>%
    filter(permno %in% avail$permno, month %in% ym_est) %>%
    group_by(permno) %>%
    summarise(n_est = sum(has_ret), .groups = "drop")
  
  # coverage in formation (require >= 48 months)
  form_cover <- panel_m %>%
    filter(permno %in% avail$permno, month %in% ym_form) %>%
    group_by(permno) %>%
    summarise(n_form = sum(has_ret), .groups = "drop")
  
  ok <- est_cover %>%
    inner_join(form_cover, by = "permno") %>%
    mutate(pass = (n_est >= length(ym_est)) & (n_form >= 48)) %>%
    summarise(n_ok = sum(pass), .groups = "drop")
  
  tibble(
    `No. of securities available` = n_available,
    `No. of securities meeting data requirement` = ok$n_ok
  )
}

# ---- 3) Apply to all 9 periods
table1_raw <- periods %>%
  rowwise() %>%
  mutate(stats = list(count_for_period(cur_data()))) %>%
  unnest(stats) %>%
  ungroup() %>%
  select(
    Period = period,
    `Portfolio formation period` = form_span,
    `Initial estimation period`  = est_span,
    `Testing period`             = test_span,
    `No. of securities available`,
    `No. of securities meeting data requirement`
  )


row_order <- c("Portfolio formation period",
               "Initial estimation period",
               "Testing period",
               "No. of securities available",
               "No. of securities meeting data requirement")


make_panel <- function(tbl, periods_vec,
                       left_colname = " ",
                       header_label = "PERIODS",
                       show_period_numbers_in_row = FALSE) {
  
  panel <- tbl %>%
    dplyr::filter(Period %in% periods_vec) %>%
    tidyr::pivot_longer(
      cols = c(
        `Portfolio formation period`,
        `Initial estimation period`,
        `Testing period`,
        `No. of securities available`,
        `No. of securities meeting data requirement`
      ),
      names_to = "Row",
      values_to = "Value",
      values_transform = list(Value = as.character),
      values_ptypes    = list(Value = character())
    ) %>%
    dplyr::mutate(
      Row    = factor(Row, levels = row_order),
      Period = factor(Period, levels = as.character(periods_vec))
    ) %>%
    dplyr::arrange(Row, Period) %>%
    tidyr::pivot_wider(names_from = Period, values_from = Value)
  
  # set column names
  names(panel)[1]  <- left_colname
  names(panel)[-1] <- as.character(periods_vec)
  
  # build header row 
  header_right <- if (show_period_numbers_in_row) as.character(periods_vec)
  else rep("", length(periods_vec))
  header_vals  <- c(header_label, header_right)
  
  # as_tibble_row expects a named LIST
  header_row <- tibble::as_tibble_row(setNames(as.list(header_vals), names(panel)))
  
  dplyr::bind_rows(header_row, panel)
}


# Table 1: Periods 1–5
table1_panelA <- make_panel(table1_raw, 1:5, show_period_numbers_in_row = TRUE)
# Table 1: Periods 6–9
table1_panelB <- make_panel(table1_raw, 6:9, show_period_numbers_in_row = TRUE)


# ---------- Export for Table 1 (Panels A & B) ----------
# Create a gt table and export it ---
export_table1 <- function(tbl, title) {
  gt_tbl <- tbl %>%
    dplyr::slice(-1) %>%                 # drop the "PERIODS / numbers" row
    gt::gt() %>%
    gt::tab_header(title = gt::md(paste0("**", title, "**"))) %>%
    gt::sub_missing(columns = gt::everything(), missing_text = "") %>%  # use sub_missing()
    gt::tab_options(
      table.font.size = gt::px(12),
      table.align = "center",
      data_row.padding = gt::px(2)
    )
  gt_tbl
}

tbl1A <- export_table1(
  table1_panelA,
  title = "Table 1. Portfolio formation, estimation, and testing periods"
)
tbl1B <- export_table1(
  table1_panelB,
  title = "Table 1. (Continued)"
)

# Save to PDF
gt::gtsave(tbl1A, "Table1.pdf")
gt::gtsave(tbl1B, "Table1 (continued).pdf")

### Export for Excel file
install.packages("writexl")
library(writexl)

write_xlsx(
  list(
    "Table1_PanelA_1-5" = table1_panelA,  
    "Table1_PanelB_6-9" = table1_panelB  
  ),
  path = "Table1_FamaMacBeth_Corrected.xlsx"
)

