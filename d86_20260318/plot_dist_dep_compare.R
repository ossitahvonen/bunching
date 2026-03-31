library(haven)
library(ggplot2)
library(dplyr)

setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

# ── Options ───────────────────────────────────────────────────────────────────
wage_var <- "nominal"
x_min    <- 2250
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

raw <- read_dta("binned_data_yearly.dta") |>
  select(year, dependants, unit, bin = all_of(bin_col)) |>
  filter(!is.na(bin), bin >= x_min)

# Build the three series, each normalised within its own group (above x_min only)
series <- bind_rows(
  raw |> filter(year == 2023, dependants == 0) |>
    mutate(density = unit / sum(unit), label = "2023, dep = 0"),
  raw |> filter(year == 2022, dependants == 0) |>
    mutate(density = unit / sum(unit), label = "2022, dep = 0"),
  raw |> filter(year == 2023, dependants == 1) |>
    mutate(density = unit / sum(unit), label = "2023, dep = 1")
)

ggplot(series, aes(x = bin, y = density, color = label)) +
  geom_line(linewidth = 0.8) +
  kink_vlines(wage_var) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(
    values = c("2023, dep = 0" = "#E41A1C", "2022, dep = 0" = "#377EB8", "2023, dep = 1" = "#4DAF4A"),
    name = NULL
  ) +
  coord_cartesian(xlim = c(x_min, NA)) +
  labs(
    x = wage_label,
    y = sprintf("Share of individuals (above %s EUR)", scales::comma(x_min)),
    title = sprintf("Income distribution above %s EUR", scales::comma(x_min))
  ) +
  theme_minimal(base_size = 13)

ggsave("income_dist_dep_compare.pdf", width = 8, height = 5)
cat("Saved income_dist_dep_compare.pdf\n")
