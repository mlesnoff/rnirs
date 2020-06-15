pca.svd <- function(X, ncomp, weights = NULL, kernel = FALSE) {
  
  ## For Kernel = FALSE
  ## SVD of sqrt(D) %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)
  
  if(!kernel) {
    res <- svd(sqrt(weights) * X, nu = 0, nv = ncomp)
    P <- res$v
    }
  else {
    res <- svd(t(sqrt(weights) * X), nu = ncomp, nv = 0)
    P <- res$u
    }
  
  T <- X %*% P
  ## If weights > 0
  ## = 1 / sqrt(weights) * res$u %*% diag(sv, nrow = ncomp)

  sv <- res$d[1:ncomp]
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




