pca.sph <- function(X, ncomp, weights = NULL) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmedspa(X, delta = .001)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  tX <- t(X)
  xnorms <- .xnorm(tX)
  tX <- scale(tX, center = FALSE, scale = xnorms)
  zX <- t(tX)
  
  z <- svd(sqrt(weights) * zX, nu = 0, nv = ncomp)
  P <- z$v
  zT <- zX %*% P
  sv <- apply(zT, MARGIN = 2, FUN = mad)
  T <- X %*% P
  
  u <- rev(order(sv))
  P <- P[, u]
  T <- T[, u]
  sv <- sv[u]
  zT <- zT[, u]

  eigs <- sv^2 
  
  row.names(zT) <- row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(zT) <- colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eigs = eigs, 
    xmeans = xmeans, weights = weights, zT = zT)

  }

