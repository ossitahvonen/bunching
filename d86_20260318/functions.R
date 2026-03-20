library(ggplot2)
library(dplyr)

# Shared kink definitions
# Pre-2023 kinks (same across all series, already in 2020 money)
kinks_old <- list(
  nominal        = c(kink1 = 1358, kink2 = 2716),
  earnings_index = c(kink1 = 1358, kink2 = 2716),
  cpi            = c(kink1 = 1358, kink2 = 2716)
)

# Post-2023 kinks (nominal: raw 2023 values; ei/cpi: deflated to 2020 money)
kinks_new <- list(
  nominal        = c(kink1 = 1846, kink2 = 3692),
  earnings_index = c(kink1 = 1688, kink2 = 3376),
  cpi            = c(kink1 = 1587, kink2 = 3174)
)

kink_vlines <- function(wage_var) {
  list(
    geom_vline(xintercept = kinks_new[[wage_var]]["kink1"], linetype = "dashed", color = "grey20", linewidth = 0.8),
    geom_vline(xintercept = kinks_new[[wage_var]]["kink2"], linetype = "dashed", color = "grey20", linewidth = 0.8),
    geom_vline(xintercept = kinks_old[[wage_var]]["kink1"], linetype = "dotted", color = "grey40", linewidth = 0.8),
    geom_vline(xintercept = kinks_old[[wage_var]]["kink2"], linetype = "dotted", color = "grey40", linewidth = 0.8)
  )
}

plot_distribution <- function(df, wage_var, wage_label, title, x_min = NULL) {
  p <- ggplot(df, aes(x = bin, y = density, color = factor(year))) +
    geom_line(linewidth = 0.8) +
    kink_vlines(wage_var) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_color_brewer(palette = "Set1", name = "Year") +
    labs(x = wage_label, y = "Share of individuals", title = title) +
    theme_minimal(base_size = 13)
  if (!is.null(x_min)) p <- p + coord_cartesian(xlim = c(x_min, NA))
  p
}

# Compare 2022->2023 change across profession classes.
# df must have columns: bin, density, year, prof_class_2019
plot_diff_classes <- function(df, wage_var, wage_label, title, x_min = NULL) {
  df_diff <- df |>
    select(prof_class_2019, year, bin, density) |>
    tidyr::pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
    filter(!is.na(y2022), !is.na(y2023)) |>
    mutate(diff_2223 = y2023 - y2022,
           class_label = paste("Class", prof_class_2019))

  p <- ggplot(df_diff, aes(x = bin, y = diff_2223, color = class_label)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    kink_vlines(wage_var) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_color_brewer(palette = "Set1", name = "Profession class") +
    labs(x = wage_label, y = "Change in share of individuals", title = title) +
    theme_minimal(base_size = 13)
  if (!is.null(x_min)) p <- p + coord_cartesian(xlim = c(x_min, NA))
  p
}

plot_diff <- function(df, wage_var, wage_label, title, x_min = NULL) {
  df_diff <- df |>
    select(year, bin, density) |>
    tidyr::pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
    filter(!is.na(y2022), !is.na(y2023)) |>
    mutate(
      diff_2223 = y2023 - y2022,
      diff_2122 = y2022 - y2021,
      diff_2021 = y2021 - y2020
    )

  p <- ggplot(df_diff, aes(x = bin)) +
    geom_line(aes(y = diff_2223, color = "2022-2023"), linewidth = 0.8) +
    geom_line(aes(y = diff_2122, color = "2021-2022"), linewidth = 0.8) +
    geom_line(aes(y = diff_2021, color = "2020-2021"), linewidth = 0.8) +
    scale_color_manual(
      values = c("2022-2023" = "#E41A1C", "2021-2022" = "#377EB8", "2020-2021" = "#4DAF4A"),
      name = NULL
    ) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    kink_vlines(wage_var) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(x = wage_label, y = "Change in share of individuals", title = title) +
    theme_minimal(base_size = 13)
  if (!is.null(x_min)) p <- p + coord_cartesian(xlim = c(x_min, NA))
  p
}
