snv <- function(X, center = TRUE, scale = TRUE) {

  X <- t(.matrix(X))
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(center) xmeans <- colMeans(X)
    else xmeans <- rep(0, p)
  
  if(scale) xscales <- (matrixStats::colVars(X) * (n - 1) / n)^.5 
    else xscales <- rep(1, p)
  
  X <- t(.scale(X, xmeans, xscales))
  
  X
  
  }




