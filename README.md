# Seasonal Mann-Kendall Trend Test (SMKTT) in R

A collection of R functions implementing the **Seasonal Mann-Kendall Trend Test** with serial correlation adjustment. This toolkit is designed for detecting monotonic trends in seasonal time-series data (e.g., hydrology, environmental monitoring, water quality) where data may exhibit serial autocorrelation across seasons.

---

## Repository Contents

| File | Description |
|------|-------------|
| `ktaub.R` | Computes **Kendall's tau-b** and **Sen's slope** for a pair of vectors |
| `serialAdjusted.R` | Adjusts the Mann-Kendall variance for **serial correlation** between seasons |
| `smktt.R` | Main entry point — runs the full **Seasonal Mann-Kendall Trend Test** |

---

## Background

The **Mann-Kendall test** is a non-parametric rank-based test for detecting monotonic trends in time series. The **Seasonal** variant (SMKTT) extends it to data collected at multiple times per year (months, quarters, etc.), computing a test statistic across all seasons jointly.

When seasons are serially correlated (i.e., measurements in one season influence another), the standard variance estimate is inflated, leading to false positives. The `serialAdjusted` function corrects for this using a covariance adjustment, following the methodology of **Hirsch & Slack (1984)** and related literature.

---

## Functions

### `ktaub(x, y)`

**File:** `ktaub.R`

Computes Kendall's tau-b correlation and Sen's slope between two numeric vectors.

**Parameters:**
- `x` — Numeric vector (typically time/year)
- `y` — Numeric vector of observed values

**Returns:** A named list:
- `tau_b` — Kendall's tau-b statistic (ranging from -1 to 1)
- `sen_slope` — Sen's slope (median of all pairwise slopes)

**Validation:**
- Stops with an error if `x` and `y` have different lengths
- Stops with an error if either vector contains `NA` values

**Example:**
```r
source("ktaub.R")
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(10, 9, 8, 7, 6.5, 6.5, 5.5, 4.5, 4.5, 3)
result <- ktaub(x, y)
# result$tau_b      → Kendall's tau-b
# result$sen_slope  → Sen's slope (trend magnitude)
```

---

### `serialAdjusted(data, SumVars, ss, alpha)`

**File:** `serialAdjusted.R`

Adjusts the p-value of the seasonal Mann-Kendall statistic to account for serial correlation between seasons using a rank-based covariance correction.

**Requires:** `matrixStats` package

**Parameters:**
- `data` — A matrix/data frame with columns: `[year, season, value]`
- `SumVars` — Sum of per-season variances (from individual Mann-Kendall tests)
- `ss` — The seasonal Mann-Kendall S statistic (use `0` to test at S=1)
- `alpha` — Significance level (e.g., `0.05`)

**Returns:** Adjusted two-tailed p-value (`sigAdj`) as a numeric scalar.

**Method:** Computes inter-season covariances from upper-triangular sign matrices and rank-based statistics, then modifies the total variance before computing the normal approximation p-value.

---

### `smktt(datain, alpha, wantplot, StartSeason = 1)`

**File:** `smktt.R`

Main function implementing the full Seasonal Mann-Kendall Trend Test pipeline.

**Parameters:**
- `datain` — A matrix/data frame with at least 3 columns: `[year, season, value]`
- `alpha` — Significance level (e.g., `0.05`)
- `wantplot` — Integer flag; set to `1` to generate a plot, `0` to suppress
- `StartSeason` — Integer indicating which season to treat as the first (default: `1`). Useful when, e.g., a water year starts in October (season 10)

**Internal helper functions:**
- `AdjustSeasons(datain, startSeason)` — Reorders seasons so the analysis year starts at `StartSeason`; calls `seasonAverage`
- `seasonAverage(datain)` — Aggregates multiple observations per season-year using the **median**, producing one value per season per year

**Processing pipeline:**
1. Seasons are reindexed if `StartSeason != 1`
2. Multiple observations per season-year are collapsed to their median
3. Per-season Mann-Kendall S statistics and variances are computed via `ktaub`
4. Seasonal statistics are aggregated: combined S, tau-b, Sen's slope
5. A **continuity correction** is applied when n < 10 years (S adjusted by ±1)
6. Serial correlation adjustment is applied via `serialAdjusted`
7. Final p-value and trend direction are returned

**Key outputs (returned list):**
- `tau_b` / `tau` — Overall Kendall's tau-b and tau statistics
- `sen_slope` — Overall Sen's slope (trend per unit time)
- `p_value` — Adjusted significance (two-tailed)
- `S` — Combined Mann-Kendall S statistic
- `sigma` — Standard deviation of S

> **Note:** `smktt.R` contains placeholder stubs for `ktaub` and `serialAdjusted` inside the function body. For the full pipeline to run, these stubs must be replaced with calls to (or sourced implementations of) `ktaub.R` and `serialAdjusted.R`.

---

## Dependencies

| Package | Used in | Purpose |
|---------|---------|---------|
| `matrixStats` | `serialAdjusted.R` | `rowRanks()` for rank-based covariance computation |

Install with:
```r
install.packages("matrixStats")
```

---

## Usage

```r
source("ktaub.R")
source("serialAdjusted.R")
source("smktt.R")

# Example: monthly streamflow data
# datain columns: year, month (1–12), value
result <- smktt(datain = my_data, alpha = 0.05, wantplot = 0, StartSeason = 1)
```

To start the analysis year in October (hydrological year):
```r
result <- smktt(datain = my_data, alpha = 0.05, wantplot = 0, StartSeason = 10)
```

---

## Known Limitations & Notes

- **No NA support:** `ktaub` stops on missing values. Rows with `NA` should be removed before calling `smktt`.
- **Stub functions in `smktt.R`:** The inner `ktaub` and `serialAdjusted` definitions inside `smktt.R` are empty stubs — the full implementations from `ktaub.R` and `serialAdjusted.R` must be sourced separately or pasted in for the code to execute end-to-end.
- **Tie correction:** `ktaub.R` uses a simplified tie correction (`sum(duplicated(...))`) rather than the full tie-group adjustment formula. Results may differ slightly from reference implementations (e.g., `Kendall::MannKendall`) for datasets with many ties.
- **Small-sample correction:** A continuity correction (S ± 1) is automatically applied when fewer than 10 years of data are present for any season.
- **Plot functionality:** The `wantplot` parameter is parsed but the plotting code is not fully implemented in the provided version.

---

## References

- Hirsch, R.M., & Slack, J.R. (1984). A nonparametric trend test for seasonal data with serial dependence. *Water Resources Research*, 20(6), 727–732.
- Sen, P.K. (1968). Estimates of the regression coefficient based on Kendall's tau. *Journal of the American Statistical Association*, 63(324), 1379–1389.
- Mann, H.B. (1945). Nonparametric tests against trend. *Econometrica*, 13(3), 245–259.

---

## License

Not specified. Please add a `LICENSE` file if distributing this code.
