pca_nipalsna <- function(X, ncomp, 
                        gs = TRUE, 
                        tol = .Machine$double.eps^0.5, 
                        maxit = 200) {
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p.col <- zdim[2]
  
    ncomp <- min(ncomp, n, p.col)
  
    xmeans <- colMeans(X, na.rm = TRUE)
    X <- .center(X, xmeans)
 
    sv <- tt <- vector(length = ncomp)
    T <- matrix(nrow = n, ncol = ncomp)
    P <- matrix(nrow = p.col, ncol = ncomp)
  
    niter <- vector(length = ncomp)
    
    s <- which(is.na(X))
    if(length(s) > 0) {
        isna <- TRUE
        ts <- which(is.na(t(X)))
        }
    else
        isna <- FALSE
    
    PtP <- matrix(0, nrow = p.col, ncol = p.col)
    TtT <- matrix(0, nrow = n, ncol = n)
    for(a in seq_len(ncomp)) {
        
        j <- which.max(colSums(abs(X), na.rm = TRUE))
        
        if(isna) {
            X0 <- replace(X, s, 0)
            t <- X0[, j]
            }
        else
            t <- X[, j]

        iter <- 1
        cont <- TRUE
        while(cont) {
     
            ## Regression of X on t
            if(isna) {
                zTT <- replace(
                    matrix(t * t, nrow = n, ncol = p.col), 
                    s, 0
                    )
                p <- crossprod(X0, t) / colSums(zTT)
                }
            else
                p <- crossprod(X, t) / sum(t * t)
            
            if(gs & a > 1)
                p <- p - PtP %*% p
            
            p <- p / sqrt(sum(p * p))
            
            zt <- t
            ## Regression of X' on p
            if(isna) {
                zPP <- replace(
                    matrix(p * p, nrow = p.col, ncol = n), 
                    ts, 0
                    )
                t = X0 %*% p / colSums(zPP)
                }
            else
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
    
        tt[a] <- sum(t * t)
        
        sv[a] <- sqrt(tt[a] / n)
        
        if(gs) {
          PtP <- PtP + tcrossprod(p)
          TtT <- TtT + tcrossprod(t) / tt[a]
          }
  
        niter[a] <- iter - 1
    
        }
 
    eig <- sv^2         
    conv <- ifelse(niter < maxit, TRUE, FALSE)
   
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
    colnames(P) <- colnames(T) <- paste("comp", 1:ncomp, sep = "") 
    
    
    if(!isna | (isna & gs))
        T.ortho <- TRUE
    else
        T.ortho <- FALSE
    
    list(T = T, P = P, R = P, sv = sv, eig = eig, 
         xmeans = xmeans, weights = rep(1 / n, n), niter = niter, 
         conv = conv, T.ortho = TRUE)
    
    }

    
