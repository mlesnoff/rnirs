pca <- function(Xr, Xu = NULL, ncomp, algo = pca.svd, ...) {
  
  Xr <- .matrix(Xr, prefix.colnam = "x")
  n <- nrow(Xr)
  
  fm <- algo(Xr, ncomp, ...)

  # xsstot = Total SS of the centered data Xr
  # = sum of the SS of the Xr-columns
  # = sum(Xr * Xr) = sum(Xr^2) 
  # = sum(apply(Xr, MARGIN = 2, FUN = sum))
  # If Xr of full rank, xsstot = sum(sv^2) where the sum is 
  # over the r singular values
  Xr <- scale(Xr, center = fm$xmeans, scale = FALSE)
  xsstot <- sum(Xr * Xr, na.rm = TRUE)
  
  xvar <- fm$xss / (n - 1)      # xss = sv^2 when Xr has no missing data
  pvar <- fm$xss / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:length(fm$sv), var = xvar, pvar = pvar, 
    cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  z <- fm$T * fm$T / (n - 1)
  contr.ind <- scale(z, center = FALSE, scale = xvar[1:ncomp])
  
  cor.circle <- cor(Xr, fm$T)

  coord.var <- crossprod(
    Xr, 
    scale(fm$T, center = FALSE, scale = fm$sv[1:ncomp])  # normalization of T
    ) / sqrt(n - 1)

  z <- coord.var^2
  contr.var <- scale(z, center = FALSE, scale = colSums(z))
  
  row.names(contr.ind) <- row.names(Xr)
  
  contr.ind <- data.frame(contr.ind)
  coord.var <- data.frame(coord.var)
  contr.var <- data.frame(contr.var)
  cor.circle <- data.frame(cor.circle)
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- .projscor(fm, .matrix(Xu))
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R,
    xmeans = fm$xmeans, weights = fm$weights, 
    explvarx = explvarx, contr.ind = contr.ind, 
    coord.var = coord.var, contr.var = contr.var,
    cor.circle = cor.circle) 
  
  }

