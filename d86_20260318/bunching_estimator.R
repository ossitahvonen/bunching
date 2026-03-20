# ── Integration interval choice ───────────────────────────────────────────────
# For each kink, the integration bounds [x_left, x_right] are chosen
# automatically using zero-crossings of the diff series (y2 - y1):
#
#   Starting from the kink point, walk left and right through the diff until
#   the first sign change is found (linearly interpolated). That crossing
#   defines the boundary of the bunching/missing-mass region.
#   If no crossing is found within `search_window` EUR, the boundary falls
#   back to kink +/- search_window.
#
# Rationale: in a clean bunching design the diff curve forms a localised
# spike or trough around each kink, returning to zero before the next feature.
#
# The plots need to be checked to make sure the limits make sense.
# ──────────────────────────────────────────────────────────────────────────────

setwd("/Users/ossitahvonen/Documents/ideation/bunching/d86_20260318")
source("functions.R")

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── Options ──────────────────────────────────────────────────────────────────
wage_var          <- "nominal"
dependants_filter <- 0
diff_years        <- c(2022, 2023)   # change from diff_years[1] -> diff_years[2]

# Max EUR distance from each kink to search for zero-crossings
search_window <- 600

# Shift all kink points by this many EUR (positive = right, negative = left; 0 = no shift)
kink_shift <- 50

# Kinks to analyse (pulled from functions.R, then shifted)
kink_old_1 <- kinks_old[[wage_var]]["kink1"] + kink_shift
kink_new_1 <- kinks_new[[wage_var]]["kink1"] + kink_shift
kink_old_2 <- kinks_old[[wage_var]]["kink2"] + kink_shift
kink_new_2 <- kinks_new[[wage_var]]["kink2"] + kink_shift

# ── Tax rates (three brackets) ────────────────────────────────────────────────
# below kink 1 | between kink 1 and kink 2 | above kink 2
t_rate_1 <- 0.66   # marginal rate below kink 1
t_rate_2 <- 0.33   # marginal rate between kink 1 and kink 2
t_rate_3 <- 0.80   # marginal rate above kink 2

# Derived: each kink's below/above pair
t_low_k1  <- t_rate_1;  t_high_k1 <- t_rate_2
t_low_k2  <- t_rate_2;  t_high_k2 <- t_rate_3

# ── Load & prepare data ───────────────────────────────────────────────────────
df_raw <- read_dta("binned_data_yearly.dta")

wage_col <- switch(wage_var,
  nominal        = "palkka_bin",
  earnings_index = "palkka_ei_bin",
  cpi            = "palkka_cpi_bin"
)

wage_label <- switch(wage_var,
  nominal        = "Monthly wage (EUR, nominal)",
  earnings_index = "Monthly wage (EUR, 2020 earnings index)",
  cpi            = "Monthly wage (EUR, 2020 CPI)"
)

df <- df_raw |>
  filter(dependants == dependants_filter, .data[[wage_col]] >= 1000) |>
  rename(bin = all_of(wage_col)) |>
  group_by(year) |>
  mutate(density = unit / sum(unit)) |>
  ungroup()

# ── Compute diff series ───────────────────────────────────────────────────────
y1 <- diff_years[1]; y2 <- diff_years[2]
col1 <- paste0("y", y1); col2 <- paste0("y", y2)

df_diff <- df |>
  filter(year %in% diff_years) |>
  select(year, bin, density) |>
  pivot_wider(names_from = year, values_from = density, names_prefix = "y") |>
  filter(!is.na(.data[[col1]]), !is.na(.data[[col2]])) |>
  mutate(diff = .data[[col2]] - .data[[col1]]) |>
  arrange(bin)

# ── Core functions ────────────────────────────────────────────────────────────

# Find first zero-crossing of `vals` starting at `from_bin`, moving left or right.
# Returns the interpolated x value of the crossing, or NA if none found within
# `max_dist` EUR of `from_bin`.
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
      # Linear interpolation to exact crossing
      if (va == vb) return(bins[a])
      t <- va / (va - vb)
      return(bins[a] + t * (bins[b] - bins[a]))
    }
  }
  return(NA_real_)
}

# Trapezoidal integration of `vals` over `bins` between x_lo and x_hi.
# Boundary bins outside the crossing are clipped to 0.
integrate_area <- function(bins, vals, x_lo, x_hi) {
  mask <- bins >= x_lo & bins <= x_hi
  b <- bins[mask]; v <- vals[mask]
  if (length(b) < 2) return(0)
  # Prepend / append boundary points at x_lo and x_hi (value interpolated to ~0 at crossing)
  if (b[1] > x_lo)        { b <- c(x_lo, b); v <- c(0, v) }
  if (tail(b, 1) < x_hi)  { b <- c(b, x_hi); v <- c(v, 0) }
  sum(diff(b) * (head(v, -1) + tail(v, 1)) / 2)
}

# Estimate the excess/missing mass around a single kink point.
# expected_sign: +1 for excess bunching (positive area), -1 for missing mass (negative area)
compute_mass <- function(df_diff, kink, expected_sign, search_window, label = "") {
  bins <- df_diff$bin
  vals <- df_diff$diff

  x_left  <- find_zero_crossing(bins, vals, kink, "left",  max_dist = search_window)
  x_right <- find_zero_crossing(bins, vals, kink, "right", max_dist = search_window)

  # Fall back to search_window boundary if no crossing found
  if (is.na(x_left))  x_left  <- kink - search_window
  if (is.na(x_right)) x_right <- kink + search_window

  area <- integrate_area(bins, vals, x_left, x_right)

  list(
    label         = label,
    kink          = kink,
    x_left        = x_left,
    x_right       = x_right,
    width         = x_right - x_left,
    area          = area,
    abs_area      = abs(area),
    expected_sign = expected_sign
  )
}

# ── Elasticity calculation ────────────────────────────────────────────────────
# For a kink at z* with rates t_low below and t_high above:
#   B     = h0(z*) * delta_z*          [Kleven eq. 2]
#   e     = (delta_z* / z*) / (delta_t / (1 - t_low))   [Kleven eq. 1]
#   => e  = (B / h0(z*) / z*) / (delta_t / (1 - t_low))
#
# Counterfactual density h0(z*):
#   old kink 2: use the *2023* density at z* (2023 is smooth there — kink moved away)
#   new kink 2: use the *2022* density at z* (2022 is smooth there — kink didn't exist yet)

compute_elasticity <- function(B, z_star, h0, t_low, t_high) {
  delta_t  <- t_high - t_low
  delta_z  <- B / h0                        # earnings response of marginal buncher
  e        <- (delta_z / z_star) / (delta_t / (1 - t_low))
  list(delta_z = delta_z, elasticity = e)
}

lookup_density <- function(df, year_val, kink) {
  d <- df |> filter(year == year_val)
  d$density[which.min(abs(d$bin - kink))]
}

# ── Run estimates ─────────────────────────────────────────────────────────────

# Kink 1 (nonconvex: rate drops 0.66->0.33; creates a hole)
# In the 2023-2022 diff: old location shows POSITIVE area (hole filled in);
#                        new location shows NEGATIVE area (new hole appeared).
res_old_k1 <- compute_mass(
  df_diff, kink_old_1, expected_sign = +1,
  search_window = search_window,
  label = sprintf("Old kink 1 (%.0f EUR) - hole filled in diff", kink_old_1)
)
res_new_k1 <- compute_mass(
  df_diff, kink_new_1, expected_sign = -1,
  search_window = search_window,
  label = sprintf("New kink 1 (%.0f EUR) - missing mass", kink_new_1)
)
h0_old_k1 <- lookup_density(df, y2, kink_old_1)
h0_new_k1 <- lookup_density(df, y1, kink_new_1)
# B = -area for old kink (what was there = negative of what disappeared in diff)
# B =  area for new kink (what appeared in diff)
elas_old_k1 <- compute_elasticity(-res_old_k1$area, kink_old_1, h0_old_k1, t_low_k1, t_high_k1)
elas_new_k1 <- compute_elasticity( res_new_k1$area, kink_new_1, h0_new_k1, t_low_k1, t_high_k1)

# Kink 2
res_old_k2 <- compute_mass(
  df_diff, kink_old_2, expected_sign = -1,
  search_window = search_window,
  label = sprintf("Old kink 2 (%.0f EUR) - missing mass", kink_old_2)
)
res_new_k2 <- compute_mass(
  df_diff, kink_new_2, expected_sign = +1,
  search_window = search_window,
  label = sprintf("New kink 2 (%.0f EUR) - excess bunching", kink_new_2)
)
h0_old_k2 <- lookup_density(df, y2, kink_old_2)
h0_new_k2 <- lookup_density(df, y1, kink_new_2)
elas_old_k2 <- compute_elasticity(-res_old_k2$area, kink_old_2, h0_old_k2, t_low_k2, t_high_k2)
elas_new_k2 <- compute_elasticity( res_new_k2$area, kink_new_2, h0_new_k2, t_low_k2, t_high_k2)

# ── Print results ─────────────────────────────────────────────────────────────
cat("\n=== Bunching / Missing Mass Estimates ===\n")
cat(sprintf("Change: %d -> %d | Wage series: %s | Dependants: %d\n\n",
            y1, y2, wage_var, dependants_filter))

print_kink_results <- function(label, pairs, t_low, t_high) {
  cat(sprintf("--- Kink %s (%.0f%% -> %.0f%%) ---\n",
              label, t_low * 100, t_high * 100))
  for (pair in pairs) {
    cat(sprintf("  %s\n", pair$res$label))
    cat(sprintf("    Zero-crossing range:    [%.1f, %.1f EUR]  (width = %.1f EUR)\n",
                pair$res$x_left, pair$res$x_right, pair$res$width))
    cat(sprintf("    Bunching mass (B):      %.5f\n", pair$res$abs_area))
    cat(sprintf("    Counterfactual h0(z*):  %.5f  (from %d density at nearest bin)\n",
                pair$h0, pair$cf_year))
    cat(sprintf("    Earnings response dz*:  %.1f EUR\n", abs(pair$elas$delta_z)))
    cat(sprintf("    Elasticity estimate:    %.3f\n\n", pair$elas$elasticity))
  }
  b_new <- pairs[[2]]$res$abs_area
  b_old <- pairs[[1]]$res$abs_area
  cat(sprintf("  Conservation check: B_new / B_old = %.3f\n", b_new / b_old))
  cat("  (1.0 = all old bunchers moved to new kink; <1 = incomplete adjustment)\n\n")
}

print_kink_results("1",
  list(list(res = res_old_k1, elas = elas_old_k1, h0 = h0_old_k1, cf_year = y2),
       list(res = res_new_k1, elas = elas_new_k1, h0 = h0_new_k1, cf_year = y1)),
  t_low_k1, t_high_k1
)
print_kink_results("2",
  list(list(res = res_old_k2, elas = elas_old_k2, h0 = h0_old_k2, cf_year = y2),
       list(res = res_new_k2, elas = elas_new_k2, h0 = h0_new_k2, cf_year = y1)),
  t_low_k2, t_high_k2
)

# ── Plot helper ───────────────────────────────────────────────────────────────
plot_bunching <- function(df_diff, res_old, res_new, elas_old, elas_new,
                          kink_old, kink_new, kink_num, wage_label, y1, y2,
                          wage_var, dependants_filter) {
  # Labels reflect what the diff shows (sign of area), not a hardcoded assumption
  lbl_old <- if (res_old$area < 0)
    sprintf("Excess bunching at old kink %d (disappeared)", kink_num)
  else
    sprintf("Missing mass at old kink %d (hole filled)", kink_num)
  lbl_new <- if (res_new$area > 0)
    sprintf("Excess bunching at new kink %d (appeared)", kink_num)
  else
    sprintf("Missing mass at new kink %d (new hole)", kink_num)

  shade_old <- df_diff |>
    filter(bin >= res_old$x_left, bin <= res_old$x_right) |>
    mutate(region = lbl_old)
  shade_new <- df_diff |>
    filter(bin >= res_new$x_left, bin <= res_new$x_right) |>
    mutate(region = lbl_new)

  x_lo <- min(res_old$x_left,  kink_old) - 200
  x_hi <- max(res_new$x_right, kink_new) + 200

  # Shade on the side where the diff actually is (sign of area tells us which side)
  ribbon_old <- if (res_old$area >= 0)
    geom_ribbon(data = shade_old, aes(ymin = 0, ymax = pmax(diff, 0), fill = region), alpha = 0.35)
  else
    geom_ribbon(data = shade_old, aes(ymin = pmin(diff, 0), ymax = 0, fill = region), alpha = 0.35)

  ribbon_new <- if (res_new$area >= 0)
    geom_ribbon(data = shade_new, aes(ymin = 0, ymax = pmax(diff, 0), fill = region), alpha = 0.35)
  else
    geom_ribbon(data = shade_new, aes(ymin = pmin(diff, 0), ymax = 0, fill = region), alpha = 0.35)

  ggplot(df_diff, aes(x = bin, y = diff)) +
    ribbon_old +
    ribbon_new +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = res_old$x_left,  linetype = "dashed",
               color = "#E41A1C", linewidth = 0.4) +
    geom_vline(xintercept = res_old$x_right, linetype = "dashed",
               color = "#E41A1C", linewidth = 0.4) +
    geom_vline(xintercept = res_new$x_left,  linetype = "dashed",
               color = "#377EB8", linewidth = 0.4) +
    geom_vline(xintercept = res_new$x_right, linetype = "dashed",
               color = "#377EB8", linewidth = 0.4) +
    geom_vline(xintercept = kink_old, linetype = "dotted",
               color = "grey30", linewidth = 0.8) +
    geom_vline(xintercept = kink_new, linetype = "dashed",
               color = "grey10", linewidth = 0.8) +
    annotate("text", x = kink_old, y = Inf, hjust = 0.5, vjust = 1.4,
             size = 3.2, color = "grey30",
             label = sprintf("Old kink %d\n%.0f EUR", kink_num, kink_old)) +
    annotate("text", x = kink_new, y = Inf, hjust = 0.5, vjust = 1.4,
             size = 3.2, color = "grey10",
             label = sprintf("New kink %d\n%.0f EUR", kink_num, kink_new)) +
    annotate("text", x = (res_old$x_left + res_old$x_right) / 2,
             y = if (res_old$area < 0) -Inf else Inf,
             vjust = if (res_old$area < 0) -0.3 else 1.8,
             size = 3, color = "#E41A1C",
             label = sprintf("B = %.4f\ndz* = %.0f EUR\ne = %.3f",
                             res_old$abs_area, abs(elas_old$delta_z), elas_old$elasticity)) +
    annotate("text", x = (res_new$x_left + res_new$x_right) / 2,
             y = if (res_new$area > 0) Inf else -Inf,
             vjust = if (res_new$area > 0) 1.8 else -0.3,
             size = 3, color = "#377EB8",
             label = sprintf("B = %.4f\ndz* = %.0f EUR\ne = %.3f",
                             res_new$abs_area, abs(elas_new$delta_z), elas_new$elasticity)) +
    scale_fill_manual(values = setNames(c("#E41A1C", "#377EB8"), c(lbl_old, lbl_new)),
                      name = NULL) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 250)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    labs(
      x     = wage_label,
      y     = "Change in share of individuals",
      title = sprintf("Bunching mass around kink %d: %d to %d (%s, dep = %d)",
                      kink_num, y1, y2, wage_var, dependants_filter)
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# ── Save plots ────────────────────────────────────────────────────────────────
p1 <- plot_bunching(df_diff, res_old_k1, res_new_k1, elas_old_k1, elas_new_k1,
                    kink_old_1, kink_new_1, 1, wage_label, y1, y2,
                    wage_var, dependants_filter)
ggsave("bunching_mass_kink1.pdf", p1, width = 11, height = 6)
cat("Plot saved: bunching_mass_kink1.pdf\n")

p2 <- plot_bunching(df_diff, res_old_k2, res_new_k2, elas_old_k2, elas_new_k2,
                    kink_old_2, kink_new_2, 2, wage_label, y1, y2,
                    wage_var, dependants_filter)
ggsave("bunching_mass_kink2.pdf", p2, width = 11, height = 6)
cat("Plot saved: bunching_mass_kink2.pdf\n")

##Claude: do not remove this code
p1 
p2