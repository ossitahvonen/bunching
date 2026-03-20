# Income Distribution & Bunching Analysis

Analysis of yearly wage distributions around Finnish earned income tax credit kink points, and how those distributions shifted when the kink points moved in 2023. Covers overall population and breakdowns by profession class and dependants.

---

## Data — `d86_20260318/`

| File | Description |
|---|---|
| `binned_data_yearly.dta` | Full population. Columns: `year`, `dependants`, `palkka_bin` (nominal), `palkka_ei_bin` (earnings index, 2020), `palkka_cpi_bin` (CPI, 2020), `unit` (N individuals), `censored` (cell < 5). |
| `binned_data_yearly_am.dta` | Same structure, adds `prof_class_2019` (profession class: 1, 2, 3). |

**Key details:**
- Years: 2020–2023. All scripts filter `bin >= 1000` (2023 has no observations below ~1000 EUR).
- Default filter: `dependants == 0`.
- Densities are normalised within each year/group so shares sum to 1.
- `censored` flag marks cells with fewer than 5 individuals (privacy threshold) — not filtered in any script.

---

## Kink points

The kinks sit at 2× and 4× the protected income portion in the earned income tax credit schedule.

| | Kink 1 | Kink 2 |
|---|---|---|
| Pre-2023 (all series, 2020 money) | 1 358 | 2 716 |
| Post-2023, nominal (raw 2023 EUR) | 1 846 | 3 692 |
| Post-2023, earnings index (2020) | 1 688 | 3 376 |
| Post-2023, CPI (2020) | 1 587 | 3 174 |

Deflators: CPI = 10355/12044; earnings index = 3313/3623.

**Tax rates (three brackets):**
- Below kink 1: 66%
- Between kink 1 and kink 2: 33%
- Above kink 2: 80%

Kink 1 is a **nonconvex** kink (rate drops → creates a hole/missing mass). Kink 2 is a **convex** kink (rate rises → creates excess bunching).

In all plots: pre-2023 kinks are **dotted** grey lines; post-2023 kinks are **dashed** dark lines.

---

## Scripts — `d86_20260318/`

All scripts start with `setwd()` and can be run from any directory.

### `functions.R` — shared library

Sourced by all other scripts. Contains kink definitions (`kinks_old`, `kinks_new`), `kink_vlines()`, and shared plot functions: `plot_distribution()`, `plot_diff()`, `plot_diff_classes()`.

---

### Distribution plots

#### `produce_graphs.R` — main distribution graphs

**Knobs at top:**
```r
wage_var          <- "earnings_index"  # "nominal" | "earnings_index" | "cpi"
dependants_filter <- 0
kink_shift        <- 50               # shift all kink lines right by N EUR (table fee correction)
```

| Output | Description |
|---|---|
| `income_dist_1A.pdf` | Distribution by year (2021/2022/2023) |
| `income_dist_1B.pdf` | Year-on-year change (2021→22 and 2022→23) |
| `income_dist_A1.pdf` | 2023 dep=0 vs 2022 dep=0 vs 2023 dep=1 (above 2250 EUR) |
| `income_dist_A2.pdf` | 2022→23 change by profession class |

#### `plot_dist.R` — overall population

```r
wage_var          <- "nominal"
dependants_filter <- 0
```

Outputs: `income_dist.pdf`, `income_dist_zoom.pdf`, `income_dist_diff.pdf`, `income_dist_diff_zoom.pdf`

#### `plot_dist_am.R` — single profession class

```r
prof_class_filter <- 1   # 1, 2, or 3
```

Outputs: `income_dist_am_class{N}.pdf`, `income_dist_diff_am_class{N}.pdf`

#### `plot_dist_am_compare.R` — all classes overlaid

2022→23 change for classes 1, 2, 3 on the same axes. Outputs: `income_dist_diff_am_compare.pdf`, `income_dist_diff_am_compare_zoom.pdf`

#### `plot_dist_dep_compare.R` — dependants comparison

Three series normalised above 2250 EUR: 2023 dep=0, 2022 dep=0, 2023 dep=1. Output: `income_dist_dep_compare.pdf`

---

### Bunching estimator

Implements the Kleven (2016) bunching estimator using the kink shift as identification: the kinks moved in 2023, so each year's distribution serves as the counterfactual for the other at each kink location.

**Integration bounds** are chosen automatically via zero-crossings of the 2022→2023 diff series starting from each kink point. The `search_window` parameter caps how far the search extends. Always inspect the shaded regions in the output plots.

**Sign convention:**
- Old kink location: diff is positive (old bunching/hole disappeared) → `B = −area`
- New kink location: diff is negative/positive (new feature appeared) → `B = area`
- With signed B and signed Δt, elasticities are positive for both convex and nonconvex kinks.

#### `bunching_estimator.R` — full population

**Knobs at top:**
```r
wage_var          <- "nominal"
dependants_filter <- 0
search_window     <- 600    # max EUR distance to search for zero-crossings
kink_shift        <- 50     # table fee correction (EUR)
```

Estimates all four kinks (old/new × kink 1/kink 2), prints results to console, and saves:

| Output | Description |
|---|---|
| `bunching_mass_kink1.pdf` | Diff plot with shaded bunching regions and elasticity annotations for kink 1 |
| `bunching_mass_kink2.pdf` | Same for kink 2 |

#### `bunching_estimator_am.R` — by profession class

Same logic, loops over profession classes 1, 2, 3 using `binned_data_yearly_am.dta`. Saves:

| Output | Description |
|---|---|
| `bunching_estimates_am.csv` | Table of all estimates (B, h0, dz*, elasticity, integration bounds) |
| `bunching_coefplot_am.pdf` | Coefficient plot: elasticity by class, old vs new kink, faceted by kink number |

---

## Gotchas

- `kink_shift` (default 50 EUR) corrects for table fees charged by employers; set to 0 to use raw kink values.
- Zoom threshold (2250 EUR) is set via `x_min` or `zoom_x` near the top of each plot script.
- The `censored` flag is not filtered — cells near sparse bins should be interpreted carefully.
