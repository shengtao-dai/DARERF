# DARERF

Distributionally Adaptive Regression Estimator under Random Forest
## Installation

```r
# Option 1: pak (recommended)
install.packages("pak")
pak::pak("shengtao-dai/DARERF")

# Option 2: remotes
install.packages("remotes")
remotes::install_github("shengtao-dai/DARERF")

# Option 3: devtools
install.packages("devtools")
devtools::install_github("shengtao-dai/DARERF")
```

## Usage
```r
library(DARERF)
#Suppose Y (numeric), X (matrix/data.frame), and x0 (numeric vector)
res <- rf_eq_estimate(Y, X, x0, nsize = 20, c_band = 1.0, K_grid = c(1,4,9),  L = 5)
str(res)
```
