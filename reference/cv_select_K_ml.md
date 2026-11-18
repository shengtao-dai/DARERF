# Cross-validated selection of the number of quantiles K (random forest)

For each fold, fits a 1000-tree quantile regression forest on the other
folds and computes the out-of-fold mean squared error of the DARE
prediction for every candidate K.

## Usage

``` r
cv_select_K_ml(Y, X, K_grid, L, h, nsize)
```

## Arguments

- Y:

  Numeric vector length n.

- X:

  Numeric matrix/data.frame n x p.

- K_grid:

  Integer vector of candidate K values.

- L:

  Integer, number of CV folds.

- h:

  Numeric bandwidth.

- nsize:

  Integer, nodesize for quantregForest.

## Value

A list with K_opt, avg_MSE, and MSE_mat.
