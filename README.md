# DARERF

Distributionally Adaptive Regression Estimator under Random Forest
## Installation

```r
# Install devtools if needed
# install.packages("devtools")

devtools::install_local("DARERF", upgrade = "never")
# or build first:
# devtools::build("DARERF"); devtools::install("DARERF_0.1.0.tar.gz")
```

## Usage
```r
library(DARERF)
#Suppose Y (numeric), X (matrix/data.frame), and x0 (numeric vector)
res <- rf_eq_estimate(Y, X, x0, nsize = 20, c_band = 1.0, K_grid = c(1,4,9),  L = 5)
str(res)
```
