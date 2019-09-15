dderiv <- function(X, n = 3, ts = 1) {

  if(n < 3) stop("n must be higher or equal to 3.")
  if(n %% 2 == 0) stop("n must be an odd integer.")
  
  X <- .matrix(X)
  colnam <- colnames(X)
  
  x2 <- X[, n:ncol(X), drop = FALSE]
  x1 <- X[, 1:ncol(x2), drop = FALSE]
  
  z <- (x2 - x1) / ts
  
  u <- n - floor(n / 2)
  colnames(z) <- colnam[u:(u + ncol(z) - 1)]
  
  z

  }
