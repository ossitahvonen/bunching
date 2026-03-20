library(haven)

setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

# ── Options ───────────────────────────────────────────────────────────────────
# wage_var: "nominal" | "earnings_index" | "cpi"
wage_var          <- "nominal"
dependants_filter <- 0
prof_class_filter <- 3   # 1, 2, or 3
# ─────────────────────────────────────────────────────────────────────────────

bin_col <- switch(wage_var,
  nominal        = "palkka_bin",
  earnings_index = "palkka_ei_bin",
  cpi            = "palkka_cpi_bin"
)

wage_label <- switch(wage_var,
  nominal        = "Yearly income, nominal (EUR)",
  earnings_index = "Yearly income, deflated by earnings index (EUR, 2020)",
  cpi            = "Yearly income, deflated by CPI (EUR, 2020)"
)

df <- read_dta("binned_data_yearly_am.dta") |>
  filter(dependants == dependants_filter, prof_class_2019 == prof_class_filter) |>
  select(year, unit, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= 1000) |>
  group_by(year) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

out_suffix <- sprintf("am_class%d", prof_class_filter)

plot_distribution(
  df, wage_var, wage_label,
  title = sprintf("Income distribution by year (prof. class %d, dependants = %d)", prof_class_filter, dependants_filter)
)
ggsave(sprintf("income_dist_%s.pdf", out_suffix), width = 8, height = 5)
cat(sprintf("Saved income_dist_%s.pdf\n", out_suffix))

plot_diff(
  df, wage_var, wage_label,
  title = sprintf("Change in income distribution (prof. class %d, dependants = %d)", prof_class_filter, dependants_filter)
)
ggsave(sprintf("income_dist_diff_%s.pdf", out_suffix), width = 8, height = 5)
cat(sprintf("Saved income_dist_diff_%s.pdf\n", out_suffix))
