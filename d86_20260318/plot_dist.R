library(haven)

setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

# ── Options ───────────────────────────────────────────────────────────────────
# wage_var: "nominal" | "earnings_index" | "cpi"
#if deflated, amounts in 2020 money
#nominal looks the best (pp limits did not change at the rate of inflation)
wage_var          <- "earnings_index"
dependants_filter <- 0
# ─────────────────────────────────────────────────────────────────────────────
###
#What are the correct limits/points for the xlines?
#in 2020 the protected portion was 679 =>first kink at 1358, second at 2716
#in 2023, the protected portion was then 923e => first kink at 1846, second 3692 (in 2023 money)
#if deflated with earnings index these become 1688 and 3376
#with cpi: first at 1587, second at 3174
## cpi deflator used: (10355/12044)
##ei deflator used: (3313/3623)

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

df <- read_dta("binned_data_yearly.dta") |>
  filter(dependants == dependants_filter) |>
  select(year, unit, bin = all_of(bin_col)) |>
  ##filter out under 1000e since 2023 does not have this info=>shares will not match by default without this
  filter(!is.na(bin), bin >= 1000) |>
  group_by(year) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

zoom_x <- 2250

plot_distribution(
  df, wage_var, wage_label,
  title = sprintf("Income distribution by year (dependants = %d)", dependants_filter)
)
ggsave("income_dist.pdf", width = 8, height = 5)
cat("Saved income_dist.pdf\n")

plot_distribution(
  df, wage_var, wage_label,
  title = sprintf("Income distribution by year (dependants = %d)", dependants_filter),
  x_min = zoom_x
)
ggsave("income_dist_zoom.pdf", width = 8, height = 5)
cat("Saved income_dist_zoom.pdf\n")

plot_diff(
  df, wage_var, wage_label,
  title = sprintf("Change in income distribution (dependants = %d)", dependants_filter)
)
ggsave("income_dist_diff.pdf", width = 8, height = 5)
cat("Saved income_dist_diff.pdf\n")

plot_diff(
  df, wage_var, wage_label,
  title = sprintf("Change in income distribution (dependants = %d)", dependants_filter),
  x_min = zoom_x
)
ggsave("income_dist_diff_zoom.pdf", width = 8, height = 5)
cat("Saved income_dist_diff_zoom.pdf\n")
