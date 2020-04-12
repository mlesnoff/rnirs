pca.eigen <- function(X, ncomp) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  xmeans <- colMeans(X)
  X <- scale(X, center = xmeans, scale = FALSE)

  z <- eigen(crossprod(X))
  
  xss <- z$values[1:ncomp]
  ## = eigenvalues of X'X
  ## = colSums(T * T)
  
  sv <- sqrt(xss)
  ## = norms of the scores T
  ## = .xnorms(T)
  ## = sqrt(colSums(T * T))
  
  P <- z$vectors[, 1:ncomp, drop = FALSE]

  T <- X %*% P

  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = rep(1, n))

}




