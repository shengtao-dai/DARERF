#' Cross-validated K selection for QRF equivalent-kernel weights
#'
#' @param Y Numeric vector length n.
#' @param X Numeric matrix/data.frame n x p.
#' @param K_grid Integer vector of candidate K values.
#' @param L Integer, number of CV folds.
#' @param h Numeric bandwidth.
#' @param nsize Integer, nodesize for quantregForest.
#' @return A list with K_opt, avg_MSE, and MSE_mat.
#' @export
cv_select_K_ml <- function(Y, X, K_grid, L, h, nsize) {
  X <- as.matrix(X)
  n <- length(Y); p <- ncol(X)
  # 修复 rep.int(length.out=) 的错误
  folds <- sample(rep_len(seq_len(L), n))
  
  MSE_mat <- matrix(NA_real_, nrow = L, ncol = length(K_grid))
  mtry <- max(1L, min(p, floor(sqrt(p))))
  
  for (l in seq_len(L)) {
    S_l        <- which(folds == l)
    S_minus_l  <- which(folds != l)
    X_train <- X[S_minus_l, , drop = FALSE]; Y_train <- Y[S_minus_l]
    X_test  <- X[S_l, , drop = FALSE];      Y_test  <- Y[S_l]
    NN <- length(Y_test)
    
    qrf_model <- quantregForest::quantregForest(
      x = X_train, y = Y_train, ntree = 1000L, mtry = mtry, nodesize = nsize, keep.inbag = TRUE
    )
    
    # 关键：使用 stats::predict（quantregForest 不导出 predict）
    condEcdf <- stats::predict(qrf_model, newdata = X_test, what = stats::ecdf)
    
    for (k_idx in seq_along(K_grid)) {
      K <- K_grid[k_idx]
      tau_grid  <- seq_len(K) / (K + 1)
      Q_hat_mat <- as.matrix(stats::predict(qrf_model, newdata = X_test, what = tau_grid))
      
      w0_matrix <- matrix(NA_real_, nrow = NN, ncol = K)
      for (i in seq_len(NN)) {
        Fi <- condEcdf[[i]]
        for (u in seq_len(K)) {
          f_plus  <- Fi(Q_hat_mat[i, u] + h)
          f_minus <- Fi(Q_hat_mat[i, u] - h)
          f_plus  <- min(max(f_plus,  0), 1)
          f_minus <- min(max(f_minus, 0), 1)
          w0_matrix[i, u] <- f_plus - f_minus
        }
      }
      
      w1 <- cbind(0, w0_matrix, 0)
      d1 <- t(diff(t(w1)))
      d2 <- t(diff(t(w1), differences = 2))
      num_w <- w0_matrix * (-d2)
      deno  <- rowSums(d1^2)
      
      w_eq <- 0.5 * sweep(num_w + num_w[, ncol(num_w):1, drop = FALSE], 1, deno, "/")
      MSE_mat[l, k_idx] <- mean((Y_test - rowSums(Q_hat_mat * w_eq))^2)
    }
  }
  
  avg_MSE <- colMeans(MSE_mat)
  list(K_opt = K_grid[which.min(avg_MSE)], avg_MSE = avg_MSE, MSE_mat = MSE_mat)
}
