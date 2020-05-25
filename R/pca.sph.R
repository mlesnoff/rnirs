pca.sph <- function(X, ncomp, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmedspa(X, delta = .001)
  X <- .center(X, xmeans)
  
  tX <- t(X)
  xnorms <- .xnorm(tX)
  tX <- .scale(tX, center = rep(0, n), xnorms)
  zX <- t(tX)
  
  z <- svd(sqrt(weights) * zX, nu = 0, nv = ncomp)
  P <- z$v
  zT <- zX %*% P
  zeig <- z$d[1:ncomp]^2
  ## = apply(zT, 2, sd)^2 * (n - 1) / n
  zxsstot <- sum(weights * zX * zX, na.rm = TRUE)
  
  T <- X %*% P
  sv <- matrixStats::colMads(T)
  eig <- sv^2 
  
  ## Here the order of loadings/scores follows zeig
  ## But an alternative: Could be re-ordered following eig (as in PCA-CR)
  ## u <- rev(order(sv))
  ## P <- P[, u]
  ## T <- T[, u]
  ## sv <- sv[u]
  
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, zT = zT, zeig = zeig,
    zxsstot = zxsstot, xmeans = xmeans, weights = weights)

  }

