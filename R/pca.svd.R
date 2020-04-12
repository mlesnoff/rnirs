pca.svd <- function(X, ncomp) {
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  xmeans <- colMeans(X)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  z <- svd(X, nu = ncomp, nv = ncomp)

  sv <- z$d[1:ncomp]  
  ## = norms of the scores T
  ## = .xnorms(T)
  ## = sqrt(colSums(T * T))
  ## If n is large, multiplying U may be time consuming
  ## The line below is not efficient (find a function for escaping %*%)
  T <- z$u %*% diag(sv, nrow = ncomp)
  
  P <- z$v

  xss <- sv^2         
  ## = eigenvalues of X'X
  ## = colSums(T * T)
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = rep(1, n))

}




