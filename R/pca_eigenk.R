pca_eigenk <- function(X, ncomp, weights = NULL) {
  
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

    zX <- sqrt(weights) * X
    res <- eigen(tcrossprod(zX), symmetric = TRUE)
    eig <- res$values[seq_len(min(n, p))]
    eig[eig < 0] <- 0
    sv <- sqrt(eig)
    P <- crossprod(zX, .scale(res$vectors[, seq_len(ncomp), drop = FALSE], 
                              scale = sv[seq_len(ncomp)]))
  
    T <- X %*% P
   
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
  
    colnames(T) <- colnames(P) <-  paste("comp", seq_len(ncomp), sep = "")
  
    list(T = T, P = P, R = P, sv = sv, eig = eig, 
        xmeans = xmeans, weights = weights, T.ortho = TRUE)

    }




