pls <- function(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...) {

  Xr <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(Xr)
  
  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")

  fm <- algo(Xr, Yr, ncomp, ...)
  
  # xsstot = Total SS of the centered data Xr
  # = sum of the SS of the Xr-columns
  # = sum(Xr * Xr) = sum(Xr^2) 
  # = sum(apply(Xr, MARGIN = 2, FUN = sum))
  Xr <- scale(Xr, center = fm$xmeans, scale = FALSE)
  xsstot <- sum(Xr * Xr)
  xss <- colSums(fm$P * fm$P) * fm$TT
  
  xvar <- xss / (n - 1)
  pvar <- xss / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:ncomp, var = xvar, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- projscor(.matrix(Xu), fm)
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, C = fm$C,
    xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights, 
    explvarx = explvarx)
  
  }

