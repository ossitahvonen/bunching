library(haven)
library(ggplot2)
library(dplyr)

df <- read_dta("binned_data_yearly.dta") |>
  filter(palkka_bin >= 1000)

# Normalize within each year to get density (proportion of total)
df <- df |>
  group_by(year) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

ggplot(df, aes(x = palkka_bin, y = density, color = factor(year))) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    labels = scales::comma,
    breaks = seq(0, 5000, by = 500)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_brewer(palette = "Set1", name = "Year") +
  labs(
    x = "Yearly income (EUR)",
    y = "Share of individuals",
    title = "Income distribution by year"
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist.pdf", width = 8, height = 5)
cat("Saved income_dist.pdf\n")

# ── Change in density ─────────────────────────────────────────────────────────
df_diff <- df |>
  select(year, palkka_bin, density) |>
  tidyr::pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
  filter(!is.na(y2021), !is.na(y2022), !is.na(y2023)) |>
  mutate(
    diff_2223 = y2023 - y2022,
    diff_2122 = y2022 - y2021
  )

ggplot(df_diff, aes(x = palkka_bin)) +
  geom_line(aes(y = diff_2223, color = "2022-2023"), linewidth = 0.8) +
  geom_line(aes(y = diff_2122, color = "2021-2022"), linewidth = 0.8) +
  scale_color_manual(
    values = c("2022-2023" = "#E41A1C", "2021-2022" = "#377EB8"),
    name = NULL
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    x = "Yearly income (EUR)",
    y = "Change in share of individuals",
    title = "Change in income distribution"
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist_diff.pdf", width = 8, height = 5)
cat("Saved income_dist_diff.pdf\n")

# ── 2022->2023 change by profession class ─────────────────────────────────────
df_am <- read_dta("../d86_20260318/binned_data_yearly_am.dta") |>
  filter(palkka_bin >= 1000, dependants == 0, prof_class_2019 %in% c(1, 2, 3)) |>
  group_by(year, prof_class_2019) |>
  mutate(density = unit / sum(unit)) |>
  ungroup() |>
  select(year, prof_class_2019, palkka_bin, density) |>
  tidyr::pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
  filter(!is.na(y2022), !is.na(y2023)) |>
  mutate(diff_2223 = y2023 - y2022,
         class_label = paste("Class", prof_class_2019))

ggplot(df_am, aes(x = palkka_bin, y = diff_2223, color = class_label)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_color_brewer(palette = "Set1", name = "Profession class") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    x = "Yearly income (EUR)",
    y = "Change in share of individuals",
    title = "Change in income distribution 2022-2023 by profession class (dependants = 0)"
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist_diff_am_compare.pdf", width = 8, height = 5)
cat("Saved income_dist_diff_am_compare.pdf\n")
