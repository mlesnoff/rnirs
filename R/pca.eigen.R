pca.eigen <- function(X, ncomp, weights = NULL) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)

  res <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
  P <- res$vectors[, 1:ncomp, drop = FALSE]
  eig <- res$values[1:ncomp]
  eig[eig < 0] <- 1e-15
  sv <- sqrt(eig)

  T <- X %*% P
  
  ## eig
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  ## = variances of scores T in metric D
  ## = colSums(weights * T * T)  
  ## = norms^2 of the scores T in metric D
  ## = .xnorm(T, weights = weights)^2
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <-  paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
    xmeans = xmeans, weights = weights, T.ortho = TRUE)

  }




