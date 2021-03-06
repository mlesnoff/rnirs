pca_svd <- function(X, ncomp, weights = NULL) {
  
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
  
    res <- svd(sqrt(weights) * X, nu = 0, nv = ncomp)
    P <- res$v
    sv <- res$d[seq_len(min(n, p))]
    sv[sv < 0] <- 0
  
    T <- X %*% P
    ## If weights > 0
    ## = 1 / sqrt(weights) * res$u %*% diag(sv, nrow = ncomp)
  
    eig <- sv^2         
    ## eig
    ## = eigenvalues of X'DX = Cov(X) in metric D 
    ## = variances of scores T in metric D
    ## = TT = colSums(weights * T * T)  
    ## = norms^2 of the scores T in metric D
    ## = .xnorm(T, weights = weights)^2   
  
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
  
    colnames(T) <- colnames(P) <- paste("comp", seq_len(ncomp), sep = "")
  
    list(T = T, P = P, R = P, sv = sv, eig = eig,
        xmeans = xmeans, weights = weights, T.ortho = TRUE)

    }




