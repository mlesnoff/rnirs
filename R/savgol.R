savgol <- function(X, m, n, p, ts = 1, ranges = NULL) {
  
  X <- .matrix(X)
  
  .f <- function(X, m, p, n, ts) {
    z <- apply(X, MARGIN = 1, FUN = signal::sgolayfilt, n = n, p = p, m = m, ts = ts)
    z <- t(z)
    colnames(z) <- colnames(X)
    row.names(z) <- row.names(X)
    z
    }

  if(is.null(ranges))
    z <- .f(X, m = m, p = p, n = n, ts = ts)
  
  else {
    k <- length(ranges)
    Xlist <- selw(X, ranges = ranges)$Xlist
    for(i in 1:k) {
      u <- Xlist[[i]]
      if(!is.null(u))
        if(ncol(u) > floor(n / 2)) u <- .f(X = u, m = m, p = p, n = n, ts = ts)
        else
          u <- NULL
      if(i == 1) z <- u else z <- cbind(z, u)
      }
    }
  
  z
  
  }

  
  
  
  
  
  
  



