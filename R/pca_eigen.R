pca_eigen <- function(X, ncomp, weights = NULL) {
  
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
  
    ncomp <- min(ncomp, n, p)
  
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)
  
    xmeans <- .xmean(X, weights = weights)
    X <- .center(X, xmeans)

    res <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
    P <- res$vectors[, 1:ncomp, drop = FALSE]
    eig <- res$values[1:min(n, p)]
    eig[eig < 0] <- 0
    sv <- sqrt(eig)

    T <- X %*% P
   
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
  
    colnames(T) <- colnames(P) <-  paste("comp", 1:ncomp, sep = "")
  
    list(T = T, P = P, R = P, sv = sv, eig = eig, 
        xmeans = xmeans, weights = weights, T.ortho = TRUE)

  }




