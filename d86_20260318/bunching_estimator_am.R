setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── Options ───────────────────────────────────────────────────────────────────
wage_var          <- "nominal"
dependants_filter <- 0
diff_years        <- c(2022, 2023)
prof_classes      <- c(1, 2, 3)

search_window <- 600
kink_shift    <- 50

kink_old_1 <- kinks_old[[wage_var]]["kink1"] + kink_shift
kink_new_1 <- kinks_new[[wage_var]]["kink1"] + kink_shift
kink_old_2 <- kinks_old[[wage_var]]["kink2"] + kink_shift
kink_new_2 <- kinks_new[[wage_var]]["kink2"] + kink_shift

# ── Tax rates ─────────────────────────────────────────────────────────────────
t_rate_1 <- 0.66; t_rate_2 <- 0.33; t_rate_3 <- 0.80
t_low_k1 <- t_rate_1; t_high_k1 <- t_rate_2
t_low_k2 <- t_rate_2; t_high_k2 <- t_rate_3

# ── Load data ─────────────────────────────────────────────────────────────────
wage_col <- switch(wage_var,
  nominal        = "palkka_bin",
  earnings_index = "palkka_ei_bin",
  cpi            = "palkka_cpi_bin"
)

df_raw <- read_dta("binned_data_yearly_am.dta") |>
  filter(dependants == dependants_filter, prof_class_2019 %in% prof_classes,
         .data[[wage_col]] >= 1000) |>
  rename(bin = all_of(wage_col))

y1 <- diff_years[1]; y2 <- diff_years[2]
col1 <- paste0("y", y1); col2 <- paste0("y", y2)

# ── Core functions (same logic as bunching_estimator.R) ───────────────────────
find_zero_crossing <- function(bins, vals, from_bin, direction, max_dist = Inf) {
  if (direction == "left") {
    idx <- rev(which(bins <= from_bin & bins >= from_bin - max_dist))
  } else {
    idx <- which(bins >= from_bin & bins <= from_bin + max_dist)
  }
  for (i in seq_len(length(idx) - 1)) {
    a <- idx[i]; b <- idx[i + 1]
    va <- vals[a]; vb <- vals[b]
    if (is.na(va) || is.na(vb)) next
    if (va * vb <= 0) {
      if (va == vb) return(bins[a])
      t <- va / (va - vb)
      return(bins[a] + t * (bins[b] - bins[a]))
    }
  }
  return(NA_real_)
}

integrate_area <- function(bins, vals, x_lo, x_hi) {
  mask <- bins >= x_lo & bins <= x_hi
  b <- bins[mask]; v <- vals[mask]
  if (length(b) < 2) return(0)
  if (b[1] > x_lo)       { b <- c(x_lo, b); v <- c(0, v) }
  if (tail(b, 1) < x_hi) { b <- c(b, x_hi); v <- c(v, 0) }
  sum(diff(b) * (head(v, -1) + tail(v, 1)) / 2)
}

compute_mass <- function(df_diff, kink, expected_sign, search_window) {
  bins <- df_diff$bin; vals <- df_diff$diff
  x_left  <- find_zero_crossing(bins, vals, kink, "left",  max_dist = search_window)
  x_right <- find_zero_crossing(bins, vals, kink, "right", max_dist = search_window)
  if (is.na(x_left))  x_left  <- kink - search_window
  if (is.na(x_right)) x_right <- kink + search_window
  area <- integrate_area(bins, vals, x_left, x_right)
  list(kink = kink, x_left = x_left, x_right = x_right,
       area = area, abs_area = abs(area))
}

compute_elasticity <- function(B, z_star, h0, t_low, t_high) {
  delta_t <- t_high - t_low
  delta_z <- B / h0
  e       <- (delta_z / z_star) / (delta_t / (1 - t_low))
  list(delta_z = delta_z, elasticity = e)
}

lookup_density <- function(df, year_val, kink) {
  d <- df |> filter(year == year_val)
  d$density[which.min(abs(d$bin - kink))]
}

# ── Loop over profession classes ───────────────────────────────────────────────
results <- list()

for (pc in prof_classes) {
  df_pc <- df_raw |>
    filter(prof_class_2019 == pc) |>
    group_by(year) |>
    mutate(density = unit / sum(unit)) |>
    ungroup()

  df_diff <- df_pc |>
    filter(year %in% diff_years) |>
    select(year, bin, density) |>
    pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
    filter(!is.na(.data[[col1]]), !is.na(.data[[col2]])) |>
    mutate(diff = .data[[col2]] - .data[[col1]]) |>
    arrange(bin)

  # ── Kink 1 ──────────────────────────────────────────────────────────────────
  # Nonconvex (rate drops): old location → positive area; new location → negative area
  res_old_k1 <- compute_mass(df_diff, kink_old_1, expected_sign = +1, search_window)
  res_new_k1 <- compute_mass(df_diff, kink_new_1, expected_sign = -1, search_window)
  h0_old_k1  <- lookup_density(df_pc, y2, kink_old_1)
  h0_new_k1  <- lookup_density(df_pc, y1, kink_new_1)
  elas_old_k1 <- compute_elasticity(-res_old_k1$area, kink_old_1, h0_old_k1, t_low_k1, t_high_k1)
  elas_new_k1 <- compute_elasticity( res_new_k1$area, kink_new_1, h0_new_k1, t_low_k1, t_high_k1)

  # ── Kink 2 ──────────────────────────────────────────────────────────────────
  # Convex (rate rises): old location → negative area; new location → positive area
  res_old_k2 <- compute_mass(df_diff, kink_old_2, expected_sign = -1, search_window)
  res_new_k2 <- compute_mass(df_diff, kink_new_2, expected_sign = +1, search_window)
  h0_old_k2  <- lookup_density(df_pc, y2, kink_old_2)
  h0_new_k2  <- lookup_density(df_pc, y1, kink_new_2)
  elas_old_k2 <- compute_elasticity(-res_old_k2$area, kink_old_2, h0_old_k2, t_low_k2, t_high_k2)
  elas_new_k2 <- compute_elasticity( res_new_k2$area, kink_new_2, h0_new_k2, t_low_k2, t_high_k2)

  rows <- list(
    list(prof_class = pc, kink = 1, timing = "old", kink_location = kink_old_1,
         t_low = t_low_k1, t_high = t_high_k1,
         B = -res_old_k1$area, h0 = h0_old_k1,
         x_left = res_old_k1$x_left, x_right = res_old_k1$x_right,
         delta_z = elas_old_k1$delta_z, elasticity = elas_old_k1$elasticity),
    list(prof_class = pc, kink = 1, timing = "new", kink_location = kink_new_1,
         t_low = t_low_k1, t_high = t_high_k1,
         B = res_new_k1$area, h0 = h0_new_k1,
         x_left = res_new_k1$x_left, x_right = res_new_k1$x_right,
         delta_z = elas_new_k1$delta_z, elasticity = elas_new_k1$elasticity),
    list(prof_class = pc, kink = 2, timing = "old", kink_location = kink_old_2,
         t_low = t_low_k2, t_high = t_high_k2,
         B = -res_old_k2$area, h0 = h0_old_k2,
         x_left = res_old_k2$x_left, x_right = res_old_k2$x_right,
         delta_z = elas_old_k2$delta_z, elasticity = elas_old_k2$elasticity),
    list(prof_class = pc, kink = 2, timing = "new", kink_location = kink_new_2,
         t_low = t_low_k2, t_high = t_high_k2,
         B = res_new_k2$area, h0 = h0_new_k2,
         x_left = res_new_k2$x_left, x_right = res_new_k2$x_right,
         delta_z = elas_new_k2$delta_z, elasticity = elas_new_k2$elasticity)
  )
  results <- c(results, rows)
}

# ── Build table ───────────────────────────────────────────────────────────────
tbl <- bind_rows(lapply(results, as.data.frame)) |>
  mutate(
    prof_class = factor(prof_class, levels = prof_classes,
                        labels = paste("Class", prof_classes)),
    kink_label = sprintf("Kink %d (%.0f%%→%.0f%%)", kink,
                         t_low * 100, t_high * 100),
    timing = factor(timing, levels = c("old", "new"),
                    labels = c("Old kink", "New kink"))
  )

write.csv(tbl, "bunching_estimates_am.csv", row.names = FALSE)
cat("Table saved: bunching_estimates_am.csv\n")

# ── Print table ───────────────────────────────────────────────────────────────
cat("\n=== Elasticity estimates by profession class ===\n")
print(
  tbl |>
    select(prof_class, kink_label, timing, kink_location, B, h0, delta_z, elasticity) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))),
  n = Inf
)

# ── Coefplot ──────────────────────────────────────────────────────────────────
# Point estimates only (no analytical SEs from this estimator).
# Y-axis: profession class. Colour: old vs new kink. Facet: kink number.

p_coef <- ggplot(tbl, aes(x = elasticity, y = prof_class, color = timing, shape = timing)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_point(size = 3.5, position = position_dodge(width = 0.4)) +
  facet_wrap(~ kink_label, scales = "free_x") +
  scale_color_manual(values = c("Old kink" = "#E41A1C", "New kink" = "#377EB8"),
                     name = NULL) +
  scale_shape_manual(values = c("Old kink" = 16, "New kink" = 17), name = NULL) +
  labs(
    x     = "Elasticity estimate (e)",
    y     = "Profession class",
    title = sprintf("Bunching elasticities by profession class (%d→%d, %s, dep = %d)",
                    y1, y2, wage_var, dependants_filter)
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.spacing = unit(1.5, "lines"))

ggsave("bunching_coefplot_am.pdf", p_coef, width = 10, height = 5)
cat("Plot saved: bunching_coefplot_am.pdf\n")

p_coef
