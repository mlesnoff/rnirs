pca.svd <- function(X, ncomp) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)

  xmeans <- colMeans(X)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  z <- svd(X, nu = ncomp, nv = ncomp)

  sv <- z$d[1:ncomp]  # norms of the scores T = sv[1:ncomp]
  
  xss <- sv^2         # = eigenvalues of X'X
   
  P <- z$v

  # If n is large, multiplying U may be time consuming
  # The line below is not efficient (find a function for escaping %*%)
  T <- z$u %*% diag(sv, nrow = ncomp)
  
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  colnames(T) <- colnames(P) <- nam
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, xmeans = xmeans, weights = rep(1 / n, n))

}




