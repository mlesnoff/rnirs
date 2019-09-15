detrend <- function(X, degree = 1, ranges = NULL) {
  
  X <- .matrix(X, prefix.colnam = "")

  .f <- function(X, degree) {
    wav <- as.numeric(colnames(X))
    fresid <- function(x, wav, degree) resid(lm(x ~ stats::poly(wav, degree)))
    z <- apply(X, MARGIN = 1, FUN = fresid, wav = wav, degree = degree)
    z <- t(z)
    colnames(z) <- colnames(X)
    row.names(z) <- row.names(X)
    z
    }

  if(is.null(ranges))
    z <- .f(X, degree = degree)
  else {
    k <- length(ranges)
    Xlist <- selw(X, ranges = ranges)$Xlist
    for(i in 1:k) {
      u <- Xlist[[i]]
      if(!is.null(u))
        if(ncol(u) > degree) u <- .f(u, degree) else u <- NULL
      if(i == 1) z <- u else z <- cbind(z, u)
      }
    }
  
  z
  
  }



