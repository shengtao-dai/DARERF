# DARERF

<!-- badges: start -->
[![R-CMD-check](https://github.com/shengtao-dai/DARERF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shengtao-dai/DARERF/actions/workflows/R-CMD-check.yaml)
[![r-universe](https://shengtao-dai.r-universe.dev/badges/DARERF)](https://shengtao-dai.r-universe.dev/DARERF)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/shengtao-dai/DARERF/blob/main/LICENSE.md)
<!-- badges: end -->

Distributionally Adaptive Regression Estimator under Random Forest.

## Overview

Least squares regression can be very inefficient when the error distribution is far from Gaussian, for example with heavy tails or outliers. The **distributionally adaptive regression estimator (DARE)** of [Dai (2025)](https://ssrn.com/abstract=5683602) estimates the conditional mean $E[Y \mid X = x_0]$ as a weighted combination of conditional quantiles at the levels $k/(K+1)$, $k = 1, \dots, K$. The weights adapt to the unknown error distribution, and the number of quantiles $K$ is chosen by cross-validation. Under regularity conditions DARE attains the asymptotic variance of the infeasible likelihood-based estimator while keeping the convergence rate of conventional estimators.

`DARERF` implements the **random forest** version of DARE. Conditional quantiles and the conditional distribution function at the target point come from a quantile regression forest ([quantregForest](https://CRAN.R-project.org/package=quantregForest)), so the method works with many covariates and nonlinear relationships without choosing a functional form.

Documentation: <https://shengtao-dai.github.io/DARERF/>

## Installation

```r
# From r-universe (no compiler needed)
install.packages("DARERF", repos = c("https://shengtao-dai.r-universe.dev", "https://cloud.r-project.org"))

# Or the development version from GitHub
# install.packages("pak")
pak::pak("shengtao-dai/DARERF")
```

## Example

```r
library(DARERF)

# Simulated data with heavy-tailed (t, 3 df) errors; the true m(0, 0) is 0.5
set.seed(123)
n <- 500
X <- cbind(rnorm(n), rnorm(n))
Y <- as.numeric(0.5 + X %*% c(1, -1) + rt(n, df = 3))

rf_eq_estimate(Y, X, x0 = c(0, 0), K_grid = c(1, 3, 5), L = 2, ntr = 500L)
#> [1] 0.3428415
```

## Main functions

| Function | Purpose |
|---|---|
| `rf_eq_estimate()` | DARE estimate of E[Y \| X = x0] using quantile regression forests |
| `cv_select_K_ml()` | Cross-validated choice of the number of quantiles K |

## Related packages

- [DARELL](https://github.com/shengtao-dai/DARELL): DARE with local linear regression
- [DARERF](https://github.com/shengtao-dai/DARERF): DARE with random forests (this package)
- [DARESR](https://github.com/shengtao-dai/DARESR): DARE with series (B-spline) expansion

## Citation

If you use `DARERF` in your work, please cite:

> Dai, S. (2025). *Distributionally Adaptive Regression Estimators*. Working paper. <https://ssrn.com/abstract=5683602>

```bibtex
@unpublished{dai2025dare,
  title  = {Distributionally Adaptive Regression Estimators},
  author = {Dai, Shengtao},
  year   = {2025},
  note   = {Working paper, SSRN 5683602},
  url    = {https://ssrn.com/abstract=5683602}
}
```

You can also run `citation("DARERF")` in R.

## License

MIT © Shengtao Dai
