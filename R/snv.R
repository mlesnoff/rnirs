snv <- function(X, center = TRUE, scale = TRUE, ranges = NULL) {

  X <- .matrix(X)
  
  if(is.null(ranges)) X <- .snv(X, center, scale)
  
  else {
    
    k <- length(ranges)
    Xlist <- selw(X, ranges)$Xlist
    for(i in 1:k) {
      z <- Xlist[[i]]
      if(!is.null(z))
        if(ncol(z) > 1) z <- .snv(z, center, scale) else z <- NULL
      if(i == 1) X <- z else X <- cbind(X, z)
      }
    
    }
  
  X
  
  }




