snv <- function(X, center = TRUE, scale = TRUE) {

  X <- t(.matrix(X))
  n <- ncol(X)
  
  if(center) xmeans <- colMeans(X) else xmeans <- rep(0, n)
  
  if(scale) xscales <- apply(X, MARGIN = 2, FUN = sd) else xscales <- rep(1, n)
  
  X <- t(scale(X, center = xmeans, scale = xscales))
  
  X
  
  }




