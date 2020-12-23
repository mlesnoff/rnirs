pca_nipals <- function(X, ncomp, 
                       weights = NULL, 
                       tol = .Machine$double.eps^0.5, 
                       maxit = 200) {
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p.col <- zdim[2]
  
    ncomp <- min(ncomp, n, p.col)
  
    if(is.null(weights))
        wgt <- rep(1 / n, n)
    else
        wgt <- weights / sum(weights)
  
    xmeans <- .xmean(X, weights = wgt)
    X <- .center(X, xmeans)
    
    sv <- vector(length = ncomp)
    T <- matrix(nrow = n, ncol = ncomp)
    P <- matrix(nrow = p.col, ncol = ncomp)
  
    niter <- vector(length = ncomp)
    
    for(a in seq_len(ncomp)) {
  
        j <- which.max(colSums(abs(X)))
        #j <- which.max(.xvar(X, weights = wgt))
        
        t <- X[, j]

        iter <- 1
        cont <- TRUE
        while(cont) {
     
            ## Regression of X on t
            if(is.null(weights))
                p <- crossprod(X, t) / sum(t * t)
            else
                p <- crossprod(wgt * X, t) / sum(wgt * t * t)
            p <- p / sqrt(sum(p * p))
            
            zt <- t
            ## Regression of X' on p
            t <- X %*% p
      
            ztol <- sum((t - zt)^2)
            
            iter <- iter + 1
            
            if(ztol < tol | iter > maxit)
                cont <- FALSE
            
            }        
        
        X <- X - tcrossprod(t, p)

        P[, a] <- p
        T[, a] <- t
    
        sv[a] <- sqrt(sum(wgt * t * t))
  
        niter[a] <- iter - 1
    
        }
      
      eig <- sv^2         
      conv <- ifelse(niter < maxit, TRUE, FALSE)
   
      row.names(T) <- row.names(X)
      row.names(P) <- colnames(X)
      colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
 
      list(T = T, P = P, R = P, sv = sv, eig = eig, 
          xmeans = xmeans, weights = wgt, niter = niter, 
          conv = conv, T.ortho = TRUE)

    }

    
