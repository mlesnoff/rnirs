pcacr <- function(X, ncomp, obj = mad, nsim = 0) {

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    xmeans <- .xmedspa(X)
    X <- .center(X, xmeans)
    
    sv <- vector(length = ncomp)
    T <- matrix(nrow = n, ncol = ncomp)
    P <- matrix(nrow = p, ncol = ncomp)
    ndir <- NULL
    
    nam <- as.character(substitute(obj))
    if(nam %in% c("mad", "sd", "var")) {
        if(nam == "mad") obj <- stats::mad
        if(nam == "sd") obj <- stats::sd
        if(nam == "var") obj <- stats::var
        fun <- switch(nam,
            mad = matrixStats::colMads,
            sd = matrixStats::colSds,
            var = matrixStats::colVars
            )
        }
        else
            fun <- function(X) apply(X, 2, obj)
    
    simpp <- .simpp.hub
    
    for(a in seq_len(ncomp)) {
        
        zP <- simpp(X, nsim = nsim, seed = 1)
        ndir[a] <- dim(zP)[2]
        
        zT <- X %*% zP
        objval <- fun(zT)
        
        zp <- zP[, which.max(objval)]
        
        zt <- zT[, which.max(objval)]
    
        X <- X - zt %*% t(zp)
        
        P[, a] <- zp
        T[, a] <- zt
        
        sv[a] <- obj(zt)
        
        }    
    
    u <- rev(order(sv))
    P <- P[, u, drop = FALSE]
    T <- T[, u, drop = FALSE]
    sv <- sv[u]    
    
    eig <- sv^2                 
     
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
    colnames(P) <- colnames(T) <- paste("comp", seq_len(ncomp), sep = "") 
    
    list(T = T, P = P, R = P, sv = sv, eig = eig, 
        xmeans = xmeans, weights = rep(1 / n, n), ndir = ndir, T.ortho = FALSE)
    
    }




