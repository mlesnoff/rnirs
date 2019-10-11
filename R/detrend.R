detrend <- function(X, degree = 1, ranges = NULL) {
  
  X <- .matrix(X, prefix.colnam = "")

  if(is.null(ranges))
    X <- .detrend(X, degree)
  
  else {
    
    k <- length(ranges)
    Xlist <- selw(X, ranges)$Xlist
    for(i in 1:k) {
      z <- Xlist[[i]]
      if(!is.null(z))
        if(ncol(z) > degree) z <- .detrend(z, degree) else z <- NULL
      if(i == 1) X <- z else X <- cbind(X, z)
      }
    
    }
  
  X
  
  }



