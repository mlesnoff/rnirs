mavg <- function(X, n = 3) {
  
  if(n < 3) stop("n must be higher or equal to 3.")
  if(n %% 2 == 0) stop("n must be an odd integer.")
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  .f <- function(x, n){
    x <- stats::filter(
      x, 
      filter = rep(1 / n, n), 
      method = "convolution", 
      sides = 2
      )
    as.vector(x)
    }
  
  X <- t(apply(X, MARGIN = 1, FUN = .f, n = n))
  dimnames(X) <- dimnam
  
  X <- X[, complete.cases(t(X)), drop = FALSE]
  
  X

  }
