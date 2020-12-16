pcasph <- function(X, ncomp, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  ncomp <- min(ncomp, n, p)
  
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
  T <- X %*% P
  sv <- matrixStats::colMads(zT)
  
  u <- rev(order(sv))
  P <- P[, u, drop = FALSE]
  T <- T[, u, drop = FALSE]
  sv <- sv[u]  
  
  eig <- sv^2 
  
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
       xmeans = xmeans, weights = weights, T.ortho = TRUE)

  }

