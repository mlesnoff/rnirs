pca <- function(Xr, Xu = NULL, ncomp, algo = pca.svd, ...) {
  
  .pca.algo <- match.fun(FUN = algo)
  
  X <- .matrix(Xr, prefix.colnam = "x")
  
  fm <- .pca.algo(X, ncomp, ...)

  T <- fm$T
  P <- fm$P
  R <- fm$R
  sv <- fm$sv
  xss <- fm$xss                 # = sv^2 when X has no missing data
  xmeans <- fm$xmeans
  
  d <- fm$weights
  
  n <- nrow(T)

  # xsstot = Total SS of the centered data X
  # = sum of the SS of the X-columns
  # = sum(X * X) = sum(X^2) 
  # = sum(apply(X, MARGIN = 2, FUN = sum))
  # If X of full rank, xsstot = sum(sv^2) where the sum is over the r singular values
  X <- scale(X, center = xmeans, scale = FALSE)
  xsstot <- sum(X * X, na.rm = TRUE)
  
  xvar <- xss / (n - 1)
  pvar <- xss / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:length(sv), var = xvar, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  z <- T * T / (n - 1)
  contr.ind <- scale(z, center = FALSE, scale = xvar[1:ncomp])
  
  cor.circle <- cor(X, T)

  z <- scale(T, center = FALSE, scale = sv[1:ncomp]) # normalization of T
  coord.var <- crossprod(X, z) / sqrt(n - 1)
  

  Tu <- NULL
  if(!is.null(Xu))
    Tu <- projscor(.matrix(Xu), fm)
  
  list(Tr = T, Tu = Tu, P = P, R = R, xmeans = xmeans, Xr = Xr, Xu = Xu, weights = d, 
    explvarx = explvarx, contr.ind = contr.ind, cor.circle = cor.circle, coord.var = coord.var)
  
  }

