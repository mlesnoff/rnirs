pls <- function(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...) {
  
  .pls.algo <- match.fun(FUN = algo)

  X <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(X)
  
  Y <- .matrix(Yr, row = FALSE)

  fm <- .pls.algo(X, Y, ncomp, ...)

  T <- fm$T
  P <- fm$P
  C <- fm$C
  R <- fm$R
  TT <- fm$TT
  xmeans <- fm$xmeans
  ymeans <- fm$ymeans
  
  d <- fm$weights
  
  # xsstot = Total SS of the centered data X
  # = sum of the SS of the X-columns
  # = sum(X * X) = sum(X^2) 
  # = sum(apply(X, MARGIN = 2, FUN = sum))
  X <- scale(X, center = xmeans, scale = FALSE)
  xsstot <- sum(X * X)
  xss <- colSums(P * P) * TT
  
  xvar <- xss / (n - 1)
  pvar <- xss / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:ncomp, var = xvar, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- projscor(.matrix(Xu), fm)
  
  list(Tr = T, Tu = Tu, P = P, C = C, R = R, xmeans = xmeans, ymeans = ymeans, 
    Xr = Xr, Xu = Xu, weights = d, explvarx = explvarx)
  
}






