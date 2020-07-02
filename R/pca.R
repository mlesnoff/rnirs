pca <- function(Xr, Xu = NULL, ncomp, algo = NULL, ...) {
  
  Xr <- .matrix(Xr)
  zdim <- dim(Xr)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(algo))
    if(n < p)
      algo <- pca.eigenk
    else
      algo <- pca.eigen
  
  fm <- algo(Xr, ncomp, ...)

  zTT <- fm$weights * fm$T * fm$T
  tt <- colSums(zTT)
  
  Xr <- .center(Xr, fm$xmeans)
  xsstot <- sum(fm$weights * Xr * Xr, na.rm = TRUE)
  ## = sum of the variances of the columns
  ## = trace of Cov(Xr)
  ## = sum(diag(crossprod(fm$weights * Xr, Xr)))
  
  pvar <- tt / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:ncomp, var = tt, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  contr.ind <- .scale(zTT, center = rep(0, ncomp), tt)
  
  xvars <- .xvar(Xr, fm$weights)
  zX <- .scale(Xr, rep(0, p), sqrt(xvars))  
  zT <- .scale(fm$T, rep(0, ncomp), sqrt(tt))
  cor.circle <- t(fm$weights * zX) %*% zT
  
  coord.var <- crossprod(
    Xr, 
    fm$weights * .scale(fm$T,  rep(0, ncomp), sqrt(tt))
    )

  z <- coord.var^2
  contr.var <- .scale(z, rep(0, ncomp), colSums(z))
  
  row.names(contr.ind) <- row.names(Xr)
  
  contr.ind <- data.frame(contr.ind)
  coord.var <- data.frame(coord.var)
  contr.var <- data.frame(contr.var)
  cor.circle <- data.frame(cor.circle)
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- .projscor(fm, .matrix(Xu))
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, eig = fm$eig,
    xmeans = fm$xmeans, weights = fm$weights, 
    explvarx = explvarx, contr.ind = contr.ind, coord.var = coord.var, 
    contr.var = contr.var, cor.circle = cor.circle, T.ortho = fm$T.ortho) 
  
  }

