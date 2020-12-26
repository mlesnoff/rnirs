pls_kernel <- function(X, Y, ncomp, weights = NULL) {
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    zp <- zdim[2]
    
    Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")     
    q <- dim(Y)[2]
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)
    
    xmeans <- .xmean(X, weights = weights) 
    X <- .center(X, xmeans)

    ymeans <- .xmean(Y, weights = weights) 
    Y <- .center(Y, ymeans)
    
    nam <- paste("comp", seq_len(ncomp), sep = "")
    T <- matrix(nrow = n, ncol = ncomp, dimnames = list(row.names(X), nam))                     
    R <- W <- P <- matrix(nrow = zp, ncol = ncomp, dimnames = list(colnames(X), nam)) 
    C <- matrix(nrow = q, ncol = ncomp, dimnames = list(colnames(Y), nam))                     
    TT <- vector(length = ncomp)
    
    Xd <- weights * X
    # = D %*% X = d * X = X * d

    XY <- crossprod(Xd, Y)
    # = t(D %*% X) %*% Y = t(X) %*% D %*% Y
    
    for(a in seq_len(ncomp)) {
    
        if(q == 1) w <- XY
            else {
                w <- XY %*% svd(t(XY), nu = 1, nv = 0)$u
                }

        w <- w / sqrt(sum(w * w))
        
        r <- w
        if(a > 1)
            for(j in seq_len(a - 1)) 
                    r <- r - sum(P[, j] * w) * R[, j]
        
        t <- X %*% r 
        
        tt <- sum(weights * t * t)         
        
        c <- crossprod(XY, r) / tt
        
        p <- crossprod(Xd, t) / tt 
        
        XY <- XY - tcrossprod(p, c) * tt    
        
        T[, a] <- t
        P[, a] <- p
        W[, a] <- w
        R[, a] <- r
        C[, a] <- c
        
        TT[a] <- tt
        
        }

    list(T = T, P = P, W = W, C = C, R = R, TT = TT,
        xmeans = xmeans, ymeans = ymeans, weights = weights, T.ortho = TRUE)
    
    }