selwik <- function(
    X, Y, ncomp, 
    algo = NULL, weights = NULL,
    nperm = 50, seed = NULL, 
    print = TRUE, 
    ...
    ) {

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    zp <- zdim[2]
    
    Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")     
    q <- dim(Y)[2]
    
    if(is.null(algo))
        algo <- pls_kernel    
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)
    
    xmeans <- .xmean(X, weights = weights) 
    X <- .center(X, xmeans)

    ymeans <- .xmean(Y, weights = weights) 
    Y <- .center(Y, ymeans)
    
    pval <- stat <- numeric()
    set.seed(seed = seed)
    for(a in seq_len(ncomp)) {
    
        if(print)
            cat(a, " ")
        
        fm <- algo(X, Y, ncomp = 1, ...)
        ## Observed covariance
        ## stat0 <- c(cov(Y, fm$T))
        ## For PLSR2
        stat0 <- sum(cov(Y, fm$T))
        
        for(i in seq_len(nperm)) {
            
            zY <- Y[sample(1:n), ]
            zfm <- algo(X, zY, ncomp = 1, ...)
            ## H0 covariance
            ## stat[i] <- c(cov(zY, zfm$T))
            ## For PLSR2
            stat[i] <- sum(cov(zY, zfm$T))
        
            }

        pval[a] <- sum(stat0 < stat) / nperm
        
        X <- X - tcrossprod(fm$T, fm$P)
        Y <- Y - tcrossprod(fm$T, fm$C)
    
        }
    set.seed(seed = NULL)
    
    if(print)
        cat("\n\n")

    list(ncomp = seq_len(ncomp), pval = pval) 

    }
