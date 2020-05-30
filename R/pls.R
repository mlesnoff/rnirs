pls <- function(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...) {

  X <- .matrix(Xr)
  n <- dim(X)[1]
  
  Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")

  fm <- algo(X, Yr, ncomp, ...)
  
  X <- .center(X, fm$xmeans)
  
  xss <- colSums(fm$P * fm$P) * fm$TT
  xsstot <- sum(fm$weights * X * X, na.rm = TRUE)
  
  pvar <- xss / xsstot
  cumpvar <- cumsum(pvar)
  xvar <- xss / n      
  
  z <- data.frame(ncomp = 1:ncomp, var = xvar, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- .projscor(fm, .matrix(Xu))
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, C = fm$C, TT = fm$TT,
    xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights, 
    explvarx = explvarx, Y = Yr, niter = fm$niter)
  
  }

