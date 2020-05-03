pca <- function(Xr, Xu = NULL, ncomp, algo = pca.svd, ...) {
  
  X <- .matrix(Xr)
  n <- dim(X)[1]
  
  fm <- algo(X, ncomp, ...)
  weights <- fm$weights

  X <- scale(X, center = fm$xmeans, scale = FALSE)
  
  eigs <- fm$eigs
  ## = variances of scores T in metric D
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  
  xsstot <- sum(weights * X * X, na.rm = TRUE)
  ## = sum of the variances of the columns
  ## = trace of Cov(X)
  ## = sum(diag(crossprod(weights * X, X)))
  
  pvar <- eigs / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:ncomp, var = eigs, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  z <- weights * fm$T * fm$T
  contr.ind <- scale(z, center = FALSE, scale = eigs)
  
  xvars <- .xvar(X, weights = weights)
  zX <- scale(X, center = FALSE, scale = sqrt(xvars))  
  zT <- scale(fm$T, center = FALSE, scale = sqrt(eigs))
  cor.circle <- t(weights * zX) %*% zT
  
  coord.var <- crossprod(
    X, 
    weights * scale(fm$T, center = FALSE, scale = sqrt(eigs))
    )

  z <- coord.var^2
  contr.var <- scale(z, center = FALSE, scale = colSums(z))
  
  row.names(contr.ind) <- row.names(X)
  
  contr.ind <- data.frame(contr.ind)
  coord.var <- data.frame(coord.var)
  contr.var <- data.frame(contr.var)
  cor.circle <- data.frame(cor.circle)
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- .projscor(fm, .matrix(Xu))
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, eigs = fm$eigs,
    xmeans = fm$xmeans, weights = fm$weights, 
    explvarx = explvarx, contr.ind = contr.ind, 
    coord.var = coord.var, contr.var = contr.var,
    cor.circle = cor.circle) 
  
  }

