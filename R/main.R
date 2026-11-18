#' Distributionally adaptive regression estimator (random forest)
#'
#' Estimates the conditional mean \eqn{m(x_0) = E[Y \mid X = x_0]} by a
#' weighted combination of quantile regression forest estimates at the levels
#' \eqn{\tau_k = k / (K + 1)}, \eqn{k = 1, \dots, K}. The weights are built
#' from the forest's conditional distribution function around each quantile,
#' and K is chosen from `K_grid` by `L`-fold cross-validation with
#' [cv_select_K_ml()].
#'
#' The bandwidth is \eqn{h = c_{band} \, n^{-1/5}} and `mtry` is
#' \eqn{\lfloor \sqrt{p} \rfloor}. The cross-validation forests always use
#' 1000 trees; `ntr` sets the number of trees of the final forest only.
#'
#' @param Y Numeric vector length n.
#' @param X Numeric matrix/data.frame n x p.
#' @param x0 Numeric length-p vector or 1-row matrix for evaluation.
#' @param nsize Integer, nodesize for quantregForest. Default 20.
#' @param c_band Numeric bandwidth multiplier. Default 1.
#' @param K_grid Integer vector of candidate K values.
#' @param L Integer, number of CV folds. Default 5.
#' @param ntr Integer, number of trees. Default 1000.
#' @return Numeric scalar, the estimate of \eqn{m(x_0)}.
#' @references Dai, S. (2025). Distributionally Adaptive Regression
#'   Estimators. Working paper. \url{https://ssrn.com/abstract=5683602}
#'
#'   Meinshausen, N. (2006). Quantile Regression Forests. Journal of Machine
#'   Learning Research, 7, 983-999.
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' X <- cbind(rnorm(n), rnorm(n))
#' Y <- as.numeric(0.5 + X %*% c(1, -1) + rt(n, df = 3))  # heavy-tailed errors
#' rf_eq_estimate(Y, X, x0 = c(0, 0), K_grid = c(1, 3, 5), L = 2, ntr = 500L)
#' # true value is 0.5
#' }
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
