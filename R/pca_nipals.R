pca_nipals <- function(X, ncomp, 
                       weights = NULL,
                       gs = TRUE,
                       tol = .Machine$double.eps^0.5, 
                       maxit = 200) {
    
    #if(any(is.na(X)))
    #    stop("\n\n Missing data (NAs) are not allowed in this NIPALS function. \n\n")
    
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
    
    sv <- tt <- vector(length = ncomp)
    T <- matrix(nrow = n, ncol = ncomp)
    P <- matrix(nrow = p.col, ncol = ncomp)
  
    niter <- vector(length = ncomp)
    PtP <- matrix(0, nrow = p.col, ncol = p.col)
    TtT <- matrix(0, nrow = n, ncol = n)
    for(a in seq_len(ncomp)) {
  
        j <- which.max(colSums(abs(X)))
        
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
            
            if(gs & a > 1)
                p <- p - PtP %*% p
            
            zt <- t
            ## Regression of X' on p
            t <- X %*% p

            if(gs & a > 1) 
                t <- t - TtT %*% t
                  
            ztol <- sum((t - zt)^2)
            
            iter <- iter + 1
            
            if(ztol < tol | iter > maxit)
                cont <- FALSE
            
            }        
        
        X <- X - tcrossprod(t, p)

        P[, a] <- p
        T[, a] <- t
        
        tt[a] <- sum(wgt * t * t)
    
        sv[a] <- sqrt(sum(wgt * t * t))
  
        if(gs) {
          PtP <- PtP + tcrossprod(p)
          TtT <- TtT + tcrossprod(t, wgt * t) / tt[a]
          }
  
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

    
