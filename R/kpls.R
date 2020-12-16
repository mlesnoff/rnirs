kpls <- function(Xr, Yr, Xu = NULL, ncomp, kern = kpol, weights = NULL, ...) {
  
  Xr <- .matrix(Xr)
  zdim <- dim(Xr)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)  
  
  fm <- kpls_nipals(Xr, Yr, ncomp, kern, weights, ...)
  
  Tu <- NULL
  if(!is.null(Xu)) {
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)
  
    Ku <- kern(Xu, Xr, ...)
    tK <- t(kern(Xr, ...))
    Kuc <- t(t(Ku - colSums(weights * t(Ku))) - colSums(weights * tK)) + 
      sum(weights * t(weights * tK))
    Tu <- Kuc %*% fm$R
    }

  list(Tr = fm$T, Tu = Tu, C = fm$C,
    weights = fm$weights, T.ortho = fm$T.ortho)

  }

