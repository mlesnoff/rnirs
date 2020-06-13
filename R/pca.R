pca <- function(Xr, Xu = NULL, ncomp, algo = pca.eigen, ...) {
  
  X <- .matrix(Xr)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  fm <- algo(X, ncomp, ...)
  weights <- fm$weights

  X <- .center(X, fm$xmeans)
  
  eig <- fm$eig
  ## = variances of scores T in metric D
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  
  xsstot <- sum(weights * X * X, na.rm = TRUE)
  ## = sum of the variances of the columns
  ## = trace of Cov(X)
  ## = sum(diag(crossprod(weights * X, X)))
  
  pvar <- eig / xsstot
  cumpvar <- cumsum(pvar)
  
  z <- data.frame(ncomp = 1:ncomp, var = eig, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  z <- weights * fm$T * fm$T
  contr.ind <- .scale(z, center = rep(0, ncomp), eig)
  
  xvars <- .xvar(X, weights = weights)
  zX <- .scale(X, rep(0, p), sqrt(xvars))  
  zT <- .scale(fm$T, rep(0, ncomp), sqrt(eig))
  cor.circle <- t(weights * zX) %*% zT
  
  coord.var <- crossprod(
    X, 
    weights * .scale(fm$T,  rep(0, ncomp), sqrt(eig))
    )

  z <- coord.var^2
  contr.var <- .scale(z, rep(0, ncomp), colSums(z))
  
  row.names(contr.ind) <- row.names(X)
  
  contr.ind <- data.frame(contr.ind)
  coord.var <- data.frame(coord.var)
  contr.var <- data.frame(contr.var)
  cor.circle <- data.frame(cor.circle)
  
  Tu <- NULL
  if(!is.null(Xu))
    Tu <- .projscor(fm, .matrix(Xu))
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, eig = fm$eig,
    xmeans = fm$xmeans, weights = fm$weights, 
    explvarx = explvarx, contr.ind = contr.ind, 
    coord.var = coord.var, contr.var = contr.var,
    cor.circle = cor.circle) 
  
  }

