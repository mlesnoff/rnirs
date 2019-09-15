pinv <- function(X, tol = sqrt(.Machine$double.eps)) {

  ## = Function ginv of MASS with some additional ouputs
  
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
    stop("'X' must be a numeric or complex matrix")
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  Xsvd <- svd(X)
  
  if (is.complex(X)) Xsvd$u <- Conj(Xsvd$u)
  
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  
  if (all(Positive)) 
    Xplus <- Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
      Xplus <- array(0, dim(X)[2L:1L])
      else
        Xplus <- Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))

  list(Xplus = Xplus, rank = sum(Positive), Xsvd = Xsvd)
  
  
  }







