library(haven)

setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

# ── Options ───────────────────────────────────────────────────────────────────
# wage_var: "nominal" | "earnings_index" | "cpi"
wage_var          <- "nominal"
dependants_filter <- 0
# ─────────────────────────────────────────────────────────────────────────────

bin_col <- switch(wage_var,
  nominal        = "palkka_bin",
  earnings_index = "palkka_ei_bin",
  cpi            = "palkka_cpi_bin"
)

wage_label <- switch(wage_var,
  nominal        = "Monthly income, nominal (EUR)",
  earnings_index = "Monthly income, deflated by earnings index (EUR, 2020)",
  cpi            = "Monthly income, deflated by CPI (EUR, 2020)"
)

df <- read_dta("binned_data_yearly_am.dta") |>
  filter(dependants == dependants_filter, prof_class_2019 %in% c(1, 2, 3)) |>
  select(year, unit, prof_class_2019, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= 1000) |>
  group_by(year, prof_class_2019) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

zoom_x <- 2250

plot_diff_classes(
  df, wage_var, wage_label,
  title = sprintf("Change in income distribution 2022-2023 by profession class (dependants = %d)", dependants_filter)
)
ggsave("income_dist_diff_am_compare.pdf", width = 8, height = 5)
cat("Saved income_dist_diff_am_compare.pdf\n")

plot_diff_classes(
  df, wage_var, wage_label,
  title = sprintf("Change in income distribution 2022-2023 by profession class (dependants = %d)", dependants_filter),
  x_min = zoom_x
)
ggsave("income_dist_diff_am_compare_zoom.pdf", width = 8, height = 5)
cat("Saved income_dist_diff_am_compare_zoom.pdf\n")
