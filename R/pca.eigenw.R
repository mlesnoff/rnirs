pca.eigenw <- function(X, ncomp, weights = rep(1, nrow(X))) {
  
  ## Eigen decomposition of t(X) %*% D %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  xmeans <- .xmeans(X, weights = weights)
  X <- scale(X, center = xmeans, scale = FALSE)

  z <- eigen(crossprod(sqrt(weights) * X))
  
  P <- z$vectors[, 1:ncomp, drop = FALSE]

  T <- X %*% P
  
  xss <- z$values[1:ncomp]
  ## = eigenvalues of X'DX
  ## = colSums(weights * T * T)
  
  sv <- sqrt(xss)
  ## = norms of the scores T (in metric D)
  ## = .xnorms(T, weights = weights)
  ## = sqrt(colSums(weights * T * T))
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <-  paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, xss = xss, 
    xmeans = xmeans, weights = weights)

  }




