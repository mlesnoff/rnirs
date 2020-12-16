outeucl <- function(X, scale = TRUE, spatial = FALSE) {
  
  X <- .matrix(X, row = FALSE)
  n <- dim(X)[1]  
  
  if(scale)
    X <- .scale(X, rep(0, n), matrixStats::colMads(X))
  
  if(!spatial)
    mu <- matrixStats::colMedians(X)
  else
    mu <- .xmedspa(X, delta = .001)  

  r <- c(sqrt(.dis(X, mu = mu)))
  r <- r / median(r)
  
  r

  }