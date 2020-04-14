pca.svd <- function(X, ncomp, weights = rep(1, nrow(X))) {
  
  ## The function implements a SVD of sqrt(D) %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  xmeans <- .xmeans(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)
  
  z <- svd(sqrt(weights) * X, nu = ncomp, nv = ncomp)
  
  sv <- z$d[1:ncomp]
  ## = norms of the scores T (in metric D)
  ## = .xnorms(T, weights = weights)
  ## = sqrt(colSums(weights * T * T))
  ## If n is large, using the "U %*%" may be time consuming
  ## The line below is not efficient (find a function for escaping %*%)
  T <- 1 / sqrt(weights) * z$u %*% diag(sv, nrow = ncomp)

  P <- z$v

  xss <- sv^2         
  ## = eigenvalues of X'DX
  ## = colSums(weights * T * T)  
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = weights)

  }




