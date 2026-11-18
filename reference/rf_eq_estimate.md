# Distributionally adaptive regression estimator (random forest)

Estimates the conditional mean \\m(x_0) = E\[Y \mid X = x_0\]\\ by a
weighted combination of quantile regression forest estimates at the
levels \\\tau_k = k / (K + 1)\\, \\k = 1, \dots, K\\. The weights are
built from the forest's conditional distribution function around each
quantile, and K is chosen from `K_grid` by `L`-fold cross-validation
with
[`cv_select_K_ml()`](https://shengtao-dai.github.io/DARERF/reference/cv_select_K_ml.md).

## Usage

``` r
rf_eq_estimate(Y, X, x0, nsize = 20, c_band = 1, K_grid, L = 5, ntr = 1000L)
```

## Arguments

- Y:

  Numeric vector length n.

- X:

  Numeric matrix/data.frame n x p.

- x0:

  Numeric length-p vector or 1-row matrix for evaluation.

- nsize:

  Integer, nodesize for quantregForest. Default 20.

- c_band:

  Numeric bandwidth multiplier. Default 1.

- K_grid:

  Integer vector of candidate K values.

- L:

  Integer, number of CV folds. Default 5.

- ntr:

  Integer, number of trees. Default 1000.

## Value

Numeric scalar, the estimate of \\m(x_0)\\.

## Details

The bandwidth is \\h = c\_{band} \\ n^{-1/5}\\ and `mtry` is \\\lfloor
\sqrt{p} \rfloor\\. The cross-validation forests always use 1000 trees;
`ntr` sets the number of trees of the final forest only.

## References

Dai, S. (2025). Distributionally Adaptive Regression Estimators. Working
paper. <https://ssrn.com/abstract=5683602>

Meinshausen, N. (2006). Quantile Regression Forests. Journal of Machine
Learning Research, 7, 983-999.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 300
X <- cbind(rnorm(n), rnorm(n))
Y <- as.numeric(0.5 + X %*% c(1, -1) + rt(n, df = 3))  # heavy-tailed errors
rf_eq_estimate(Y, X, x0 = c(0, 0), K_grid = c(1, 3, 5), L = 2, ntr = 500L)
#> [1] 0.6542657
# true value is 0.5
# }
```
