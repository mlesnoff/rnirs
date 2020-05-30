out.eucl <- function(X, scale = TRUE) {
  
  X <- .matrix(X, row = FALSE)
  n <- dim(X)[1]  
  
  if(scale)
    X <- .scale(X, rep(1, n), matrixStats::colMads(X))
  
  mu <- matrixStats::colMedians(X)

  r <- c(sqrt(.dis(X, mu = mu)))
  
  r

  }