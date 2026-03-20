# Handoff: Income Distribution / Bunching Analysis

## Project goal

Analyse the distribution of yearly wages around tax kink points, and how that distribution shifted across years (2020–2023). The key event is a change in the protected income portion in 2023, which moved the kink points upward. The analysis looks at the overall population as well as by profession class and dependants count.

---

## Data folder: `d86_20260318/`

### Files

| File | Description |
|---|---|
| `binned_data_yearly.dta` | Full population. Columns: `year`, `dependants`, `palkka_bin` (nominal), `palkka_ei_bin` (earnings index, 2020), `palkka_cpi_bin` (CPI, 2020), `unit` (N individuals), `censored` (cell < 5 individuals). |
| `binned_data_yearly_am.dta` | Same structure as above but adds `prof_class_2019` (profession class: 1, 2, 3). All three wage bin columns are populated. |

### Key data details

- **Years**: 2020, 2021, 2022, 2023. Note: 2023 does not have observations below ~1000 EUR, so all scripts filter to `bin >= 1000` to keep shares comparable across years.
- **Dependants**: 0–4. Default filter is `dependants == 0` throughout. Density shares are always normalised within the filtered group.
- **Profession class** (`prof_class_2019`): 1, 2, or 3. Only present in `_am` file.
- **Wage variables**: `palkka_bin` = nominal; `palkka_ei_bin` = deflated by earnings index (base 2020); `palkka_cpi_bin` = deflated by CPI (base 2020).

---

## Kink points

The kinks mark the first and second tax kink in the earned income tax credit schedule. They sit at 2× and 4× the protected income portion.

| | Kink 1 | Kink 2 |
|---|---|---|
| **Pre-2023** (all series, 2020 money) | 1 358 | 2 716 |
| **Post-2023, nominal** (raw 2023 EUR) | 1 846 | 3 692 |
| **Post-2023, earnings index** (2020 money) | 1 688 | 3 376 |
| **Post-2023, CPI** (2020 money) | 1 587 | 3 174 |

Deflators used: CPI = 10355/12044; earnings index = 3313/3623.

In the plots:
- Pre-2023 kinks: **dotted** grey lines
- Post-2023 kinks: **dashed** dark lines

---

## R scripts

All scripts live in `d86_20260318/` and start with `setwd()` pointing there, so run them from any working directory.

### `functions.R` — shared library

Sourced by all other scripts. Contains:
- `kinks_old` / `kinks_new` lists (the values above)
- `kink_vlines(wage_var)` — returns the four `geom_vline` layers
- `plot_distribution(df, wage_var, wage_label, title, x_min = NULL)` — density line plot, one line per year
- `plot_diff(df, wage_var, wage_label, title, x_min = NULL)` — overlaid year-on-year change lines (2020→21, 2021→22, 2022→23)
- `plot_diff_classes(df, wage_var, wage_label, title, x_min = NULL)` — 2022→23 change overlaid across profession classes

The `x_min` argument applies `coord_cartesian(xlim = c(x_min, NA))` for zoomed versions.

---

### `plot_dist.R` — overall population

**Options at top:**
```r
wage_var          <- "nominal"   # "nominal" | "earnings_index" | "cpi"
dependants_filter <- 0
```

**Outputs:**
| File | Description |
|---|---|
| `income_dist.pdf` | Distribution by year, full x range |
| `income_dist_zoom.pdf` | Same, zoomed to x >= 2250 |
| `income_dist_diff.pdf` | Year-on-year changes, full x range |
| `income_dist_diff_zoom.pdf` | Same, zoomed to x >= 2250 |

---

### `plot_dist_am.R` — single profession class

**Options at top:**
```r
wage_var          <- "nominal"
dependants_filter <- 0
prof_class_filter <- 1   # 1, 2, or 3
```

**Outputs** (suffix = `am_class{N}`):
| File | Description |
|---|---|
| `income_dist_am_class{N}.pdf` | Distribution by year for class N |
| `income_dist_diff_am_class{N}.pdf` | Year-on-year changes for class N |

---

### `plot_dist_am_compare.R` — all profession classes overlaid

**Options at top:**
```r
wage_var          <- "nominal"
dependants_filter <- 0
```

Shows all three profession classes on the same plot for the 2022→2023 change.

**Outputs:**
| File | Description |
|---|---|
| `income_dist_diff_am_compare.pdf` | 2022→23 change by class, full x range |
| `income_dist_diff_am_compare_zoom.pdf` | Same, zoomed to x >= 2250 |

---

### `plot_dist_dep_compare.R` — dependants comparison

Single plot overlaying three series, all normalised above 2250 EUR:
1. 2023 distribution, dependants = 0
2. 2022 distribution, dependants = 0
3. 2023 distribution, dependants = 1

**Options at top:**
```r
wage_var <- "nominal"
x_min    <- 2250
```

**Output:** `income_dist_dep_compare.pdf`

---

## Notes / gotchas

- The `censored` flag marks cells with fewer than 5 individuals (privacy threshold). Not currently filtered out in any script — worth keeping in mind for inference near sparse bins.
- 2023 data starts at ~1000 EUR, so the `bin >= 1000` filter is essential for comparable density shares.
- The zoom threshold (2250 EUR) is set via `zoom_x` near the top of each script.
- Density is always normalised within year (and within class/dependants group where applicable), so shares sum to 1 within each line.
