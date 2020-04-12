pca <- function(Xr, Xu = NULL, ncomp, algo = pca.svd, ...) {
  
  X <- .matrix(Xr)
  n <- nrow(X)
  
  fm <- algo(X, ncomp, ...)
  weights <- fm$weights

  X <- scale(X, center = fm$xmeans, scale = FALSE)
  xss <- fm$xss 
  xsstot <- sum(weights * X * X, na.rm = TRUE)
  
  pvar <- xss / xsstot
  cumpvar <- cumsum(pvar)
  xvar <- .xvars(fm$T, weights = weights)      
  
  z <- data.frame(ncomp = 1:ncomp, var = xvar, pvar = pvar, cumpvar = cumpvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  z <- weights * fm$T * fm$T
  contr.ind <- scale(z, center = FALSE, scale = fm$xss)
  
  zvars <- .xvars(X, weights = weights)
  zX <- scale(X, center = FALSE, scale = sqrt(zvars))  
  zvars <- .xvars(fm$T, weights = weights)
  zT <- scale(fm$T, center = FALSE, scale = sqrt(zvars))
  cor.circle <- t(weights * zX) %*% zT  / sum(weights)
  
  coord.var <- crossprod(
    X, 
    sqrt(weights / sum(weights)) * scale(fm$T, center = FALSE, scale = sqrt(xss))
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
  
  list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R,
    xmeans = fm$xmeans, weights = fm$weights, 
    explvarx = explvarx, contr.ind = contr.ind, 
    coord.var = coord.var, contr.var = contr.var,
    cor.circle = cor.circle) 
  
  }

