pca.eigen <- function(X, ncomp) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  
  d <- rep(1 /n, n)

  xmeans <- colMeans(X)
  X <- scale(X, center = xmeans, scale = FALSE)

  z <- eigen(crossprod(X))
  
  xss <- z$values[1:ncomp]  # = eigenvalues
  
  sv <- sqrt(xss)           # norms of the scores T = sv[1:ncomp]
  
  P <- z$vectors[, 1:ncomp, drop = FALSE]

  T <- X %*% P

  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  colnames(T) <- colnames(P) <- nam
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, xmeans = xmeans, weights = d)

}




