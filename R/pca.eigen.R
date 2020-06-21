pca.eigen <- function(X, ncomp, weights = NULL, kern = FALSE) {
  
  ## For Kernel = FALSE
  ## Eigen decomposition of t(X) %*% D %*% X
  ## where D = diag(weights) and X has been centered with metric D
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  xmeans <- .xmean(X, weights = weights)
  X <- .center(X, xmeans)

  if(!kern) {
    res <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
    P <- res$vectors[, 1:ncomp, drop = FALSE]
    eig <- res$values[1:ncomp]
    sv <- sqrt(eig)
    }
  else {
    res <- eigen(tcrossprod(sqrt(weights) * X), symmetric = TRUE) # 
    eig <- res$values[1:ncomp]
    sv <- sqrt(eig)
    P <- crossprod(sqrt(weights) * X, 
      .scale(res$vectors[, 1:ncomp, drop = FALSE], scale = sv))
    }

  T <- X %*% P
  
  ## eig
  ## = eigenvalues of X'DX = Cov(X) in metric D 
  ## = variances of scores T in metric D
  ## = colSums(weights * T * T)  
  
  ## sv
  ## = norms of the scores T in metric D
  ## = .xnorm(T, weights = weights)
  ## = sqrt(colSums(weights * T * T))
   
  row.names(T) <- row.names(X)
  row.names(P) <- colnames(X)
  
  colnames(T) <- colnames(P) <-  paste("comp", 1:ncomp, sep = "")
  
  list(T = T, P = P, R = P, sv = sv, eig = eig, 
    xmeans = xmeans, weights = weights, T.ortho = TRUE)

  }




