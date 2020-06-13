pca.eigen <- function(X, ncomp, weights = NULL, kernel = FALSE) {
  
  ## Eigen decomposition of t(X) %*% D %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- nrow(X)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)

  if(!kernel) {
    res <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
    P <- res$vectors[, 1:ncomp, drop = FALSE]
    }
  else {
    res <- eigen(tcrossprod(sqrt(weights) * X), symmetric = TRUE) # 
    z <- 1 /  sqrt(res$values[1:ncomp])
    P <- crossprod(sqrt(weights) * X, res$vectors[, 1:ncomp, drop = FALSE]) %*% diag(z)
    }

  T <- X %*% P
  
  eig <- res$values[1:ncomp]
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  ## = variances of scores T in metric D
  ## = colSums(weights * T * T)  
  
  sv <- sqrt(eig)
  ## = norms of the scores T in metric D
  ## = .xnorm(T, weights = weights)
  ## = sqrt(colSums(weights * T * T))
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <-  paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
    xmeans = xmeans, weights = weights)

  }




