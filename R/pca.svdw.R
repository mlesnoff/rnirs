pca.svdw <- function(X, ncomp, weights = rep(1, nrow(X))) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  p <- ncol(X)
  
  d <- weights / sum(weights)
  
  xmeans <- crossprod(d, X)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  z <- svd(sqrt(d) * X, nu = ncomp, nv = ncomp)

  sv <- z$d[1:ncomp]
  
  P <- z$v

  # If n is large, using the "U %*%" may be time consuming
  # The line below is not efficient (find a function for escaping %*%)
  
  T <- 1 / sqrt(d) * z$u %*% diag(sv, nrow = ncomp)

  xss <- colSums(T * T)
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  nam <- paste("comp", 1:ncomp, sep = "")
  colnames(T) <- colnames(P) <- nam
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, xmeans = xmeans, weights = d)

}




