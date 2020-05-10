pinv <- function(X, tol = sqrt(.Machine$double.eps)) {

  ## = Function ginv of MASS with some additional ouputs
  
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
    stop("'X' must be a numeric or complex matrix")
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  fm <- svd(X)
  
  if (is.complex(X)) fm$u <- Conj(fm$u)
  
  Positive <- fm$d > max(tol * fm$d[1L], 0)
  
  if (all(Positive)) 
    Xplus <- fm$v %*% (1/fm$d * t(fm$u))
    else if (!any(Positive)) 
      Xplus <- array(0, dim(X)[2L:1L])
      else
        Xplus <- fm$v[, Positive, drop = FALSE] %*% ((1/fm$d[Positive]) * t(fm$u[, Positive, drop = FALSE]))

  list(Xplus = Xplus, rank = sum(Positive), fm = fm)
  
  
  }







