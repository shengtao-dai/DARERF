#' Distributionally Adaptive Regression Estimator via Random Forest
#'
#' @param Y Numeric vector length n.
#' @param X Numeric matrix/data.frame n x p.
#' @param x0 Numeric length-p vector or 1-row matrix for evaluation.
#' @param nsize Integer, nodesize for quantregForest. Default 20.
#' @param c_band Numeric bandwidth multiplier. Default 1.
#' @param K_grid Integer vector of candidate K values.
#' @param L Integer, number of CV folds. Default 5.
#' @param ntr Integer, number of trees. Default 1000.
#' @return Numeric scalar estimate m(x0).
#' @export
rf_eq_estimate <- function(Y, X, x0, nsize = 20, c_band = 1, K_grid, L = 5, ntr = 1000L) {
  X <- as.matrix(X)
  x0m <- if (is.null(dim(x0))) matrix(x0, nrow = 1) else x0
  
  n <- nrow(X); p <- ncol(X)
  mtry <- max(1L, min(p, floor(sqrt(p))))
  h <- c_band * n^(-1/5)
  
  qrf_model <- quantregForest::quantregForest(
    x = X, y = Y, ntree = ntr, mtry = mtry, nodesize = nsize, keep.inbag = TRUE
  )
  
  # 关键：stats::predict + stats::ecdf
  Fi <- stats::predict(qrf_model, newdata = x0m, what = stats::ecdf)
  if (is.list(Fi)) Fi <- Fi[[1]]
  
  cv_rf <- cv_select_K_ml(Y, X, K_grid, L, h, nsize)
  K <- cv_rf$K_opt
  tau_grid <- seq_len(K) / (K + 1)
  Q_hat <- as.numeric(stats::predict(qrf_model, newdata = x0m, what = tau_grid))
  
  w0 <- vapply(Q_hat, function(q) {
    f_plus  <- Fi(q + h); f_minus <- Fi(q - h)
    min(max(f_plus, 0), 1) - min(max(f_minus, 0), 1)
  }, numeric(1))
  
  w1 <- c(0, w0, 0)
  d1 <- diff(w1); d2 <- diff(w1, differences = 2)
  num <- w0 * (-d2); den <- sum(d1^2)
  w_eq <- 0.5 * (num + rev(num)) / den
  
  sum(Q_hat * w_eq)
}
