pca.svd <- function(X, ncomp, weights = NULL) {
  
  ## The function implements a SVD of sqrt(D) %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)
  
  z <- svd(sqrt(weights) * X, nu = 0, nv = ncomp)
  
  P <- z$v
  
  T <- X %*% P
  ## If weights > 0
  ## = 1 / sqrt(weights) * z$u %*% diag(sv, nrow = ncomp)

  sv <- z$d[1:ncomp]
  ## = norms of the scores T in metric D
  ## = .xnorm(T, weights = weights)
  ## = sqrt(colSums(weights * T * T))

  eig <- sv^2         
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  ## = variances of scores T in metric D
  ## = colSums(weights * T * T)  
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <- paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig,
    xmeans = xmeans, weights = weights)

  }




