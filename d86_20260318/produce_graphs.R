library(haven)

#setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
setwd("//ad.helsinki.fi/home/o/osstahv/Documents/GitHub/bunching/d86_20260318")
source("functions.R")

# ── Knobs ──────────────────────────────────────────────────────────────────────
# wage_var: "nominal" | "earnings_index" | "cpi"  (deflated = 2020 money)
wage_var          <- "earnings_index"
dependants_filter <- 0    # used for graphs 1A and 1B

# Kink lines (pre-2023 = dotted, post-2023 = dashed)
kink_old_1 <- 1327  ##earnings index, nominal =1392
kink_old_2 <-  2654  # earnings index, nominal=2716
kink_new_1 <- 1688   # earnings index default; nominal = 1846, cpi = 1587
kink_new_2 <- 3376   # earnings index default; nominal = 3692, cpi = 3174

# Shift all kink lines by this many EUR (positive = right, negative = left; 0 = no shift)
# In case the user wants to correct for the table fees
kink_shift <- 50
# ──────────────────────────────────────────────────────────────────────────────

# Override kink lists from functions.R with the values set above (plus shift)
kinks_old[[wage_var]] <- c(kink1 = kink_old_1 + kink_shift, kink2 = kink_old_2 + kink_shift)
kinks_new[[wage_var]] <- c(kink1 = kink_new_1 + kink_shift, kink2 = kink_new_2 + kink_shift)
# Graphs produced:
#   1A  income_dist_1A.pdf   Distribution 2021/2022/2023 (dep = dependants_filter)
#   1B  income_dist_1B.pdf   Change 2021->2022 and 2022->2023
#   A1  income_dist_A1.pdf   Comparison: 2023 dep=0, 2022 dep=0, 2023 dep=1 (above 2250)
#   A2  income_dist_A2.pdf   Change 2022->2023 by profession class (dep=0)
# ──────────────────────────────────────────────────────────────────────────────

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

# ── 1A & 1B: overall population ───────────────────────────────────────────────
df <- read_dta("binned_data_yearly.dta") |>
  filter(dependants == dependants_filter) |>
  select(year, unit, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= 1000, year %in% c(2021, 2022, 2023)) |>
  group_by(year) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

# 1A
plot_distribution(
  df, wage_var, wage_label,
  title = sprintf("Income distribution by year (dependants = %d)", dependants_filter)
)
ggsave("income_dist_1A.pdf", width = 8, height = 5)
cat("Saved income_dist_1A.pdf\n")

# 1B
df_1B <- df |>
  select(year, bin, density) |>
  tidyr::pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
  filter(!is.na(y2022), !is.na(y2023)) |>
  mutate(diff_2223 = y2023 - y2022, diff_2122 = y2022 - y2021)

ggplot(df_1B, aes(x = bin)) +
  geom_line(aes(y = diff_2223, color = "2022-2023"), linewidth = 0.8) +
  geom_line(aes(y = diff_2122, color = "2021-2022"), linewidth = 0.8) +
  scale_color_manual(
    values = c("2022-2023" = "#E41A1C", "2021-2022" = "#377EB8"), name = NULL
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  kink_vlines(wage_var) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    x = wage_label, y = "Change in share of individuals",
    title = sprintf("Change in income distribution (dependants = %d)", dependants_filter)
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist_1B.pdf", width = 8, height = 5)
cat("Saved income_dist_1B.pdf\n")

# ── A1: comparison graph, normalised above 2250 ───────────────────────────────
x_min_compare <- 2250

raw <- read_dta("binned_data_yearly.dta") |>
  select(year, dependants, unit, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= x_min_compare)

series <- bind_rows(
  raw |> filter(year == 2023, dependants == 0) |>
    mutate(density = unit / sum(unit), label = "2023, dep = 0"),
  raw |> filter(year == 2022, dependants == 0) |>
    mutate(density = unit / sum(unit), label = "2022, dep = 0"),
  raw |> filter(year == 2023, dependants == 1) |>
    mutate(density = unit / sum(unit), label = "2023, dep = 1")
)

ggplot(series, aes(x = bin, y = density, color = label, linetype = label)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(
    values = c("2023, dep = 0" = "solid", "2022, dep = 0" = "solid", "2023, dep = 1" = "dashed"),
    name = NULL
  ) +
  kink_vlines(wage_var) +
  coord_cartesian(xlim = c(x_min_compare, NA)) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(
    values = c("2023, dep = 0" = "#E41A1C", "2022, dep = 0" = "#377EB8", "2023, dep = 1" = "#4DAF4A"),
    name = NULL
  ) +
  labs(
    x = wage_label,
    y = sprintf("Share of individuals (above %s EUR)", scales::comma(x_min_compare)),
    title = sprintf("Income distribution above %s EUR", scales::comma(x_min_compare))
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist_A1.pdf", width = 8, height = 5)
cat("Saved income_dist_A1.pdf\n")

# ── A2: 2022->2023 change by profession class ─────────────────────────────────
df_am <- read_dta("binned_data_yearly_am.dta") |>
  filter(dependants == 0, prof_class_2019 %in% c(1, 2, 3)) |>
  select(year, unit, prof_class_2019, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= 1000) |>
  group_by(year, prof_class_2019) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

plot_diff_classes(
  df_am, wage_var, wage_label,
  title = "Change in income distribution 2022-2023 by profession class (dep = 0)"
)
ggsave("income_dist_A2.pdf", width = 8, height = 5)
cat("Saved income_dist_A2.pdf\n")
