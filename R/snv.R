snv <- function(X, center = TRUE, scale = TRUE, ranges = NULL) {

  X <- .matrix(X)
  
  .snv <- function(X, center, scale) {
    zX <- t(X)
    n <- ncol(zX)
    if(center) xmeans <- colMeans(zX) else xmeans <- rep(0, n)
    if(scale) xscales <- apply(zX, MARGIN = 2, FUN = sd) else xscales <- rep(1, n)
    zX <- scale(zX, center = xmeans, scale = xscales)
    t(zX)
    }

  if(is.null(ranges)) z <- .snv(X, center = center, scale = scale)
  
  else {
    
    k <- length(ranges)
    Xlist <- selw(X, ranges = ranges)$Xlist
    for(i in 1:k) {
      u <- Xlist[[i]]
      if(!is.null(u))
        if(ncol(u) > 1) u <- .snv(X = u, center = center, scale = scale) else u <- NULL
      if(i == 1) z <- u else z <- cbind(z, u)
      }
    
    }
  
  z
  
  }




