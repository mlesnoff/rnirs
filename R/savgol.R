savgol <- function(X, m, n, p, ts = 1, ranges = NULL) {
  
  X <- .matrix(X)
  
  if(is.null(ranges))
    X <- .savgol(X, m, p, n, ts)
  
  else {
    k <- length(ranges)
    Xlist <- selw(X, ranges = ranges)$Xlist
    for(i in 1:k) {
      z <- Xlist[[i]]
      if(!is.null(z))
        if(ncol(z) > floor(n / 2)) z <- .savgol(z, m, p, n, ts)
        else
          z <- NULL
      if(i == 1) X <- z else X <- cbind(X, z)
      }
    }
  
  X
  
  }

