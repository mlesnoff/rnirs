savgol <- function(X, m, n, p, ts = 1) {
  
  X <- .matrix(X)
  dimnam <- dimnames(X)
  
  X <- t(apply(X, MARGIN = 1, 
    FUN = signal::sgolayfilt, n = n, p = p, m = m, ts = ts))

  dimnames(X) <- dimnam
  
  X
  
  }

