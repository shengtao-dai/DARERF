test_that("functions exist", {
  expect_true(exists("rf_eq_estimate"))
  expect_true(exists("cv_select_K_ml"))
})

test_that("basic run on small data", {
  set.seed(123)
  n <- 500
  X <- cbind(rnorm(n), rnorm(n))
  beta <- c(1, -1)
  Y <- as.numeric(0.5 + X %*% beta + rnorm(n))
  x0 <- c(0, 0)
  K_grid <- c(1, 4, 9)
  
  expect_error(
    rf_eq_estimate(Y, X, x0, nsize = 20, c_band = 1.0, K_grid = K_grid, L = 2, ntr = 500L),
    NA
  )
})
