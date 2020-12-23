dderiv <- function(X, n = 3, ts = 1) {

  if(n < 3)
    stop("n must be higher or equal to 3.")
  if(n %% 2 == 0)
    stop("n must be an odd integer.")
  
  X <- .matrix(X)
  colnam <- colnames(X)

  zX <- X[, n:ncol(X), drop = FALSE]
  X <- (zX - X[, seq_len(ncol(zX)), drop = FALSE]) / ts 
  
  u <- n - floor(n / 2)
  colnames(X) <- colnam[seq(u, (u + ncol(X) - 1))]
  
  X

  }
