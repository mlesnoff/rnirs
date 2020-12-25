cvpcarw <- function(X, ncomp, algo = NULL, segm, 
                                    print = TRUE, ...) {
    
    ### Row-wise K-fold (rkf) algorithm
    ### = "Algorithm1" in Camacho & Ferrer 2012
    ### See also Bro et al. 2008
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq(0, ncomp)
    
    if(is.null(algo))
        if(n < p)
            algo <- pca_eigenk
        else
            algo <- pca_eigen
    
    N <- n * p
    df <- n * zncomp 
    df.ssr <- N - df
    
    nrep <- length(segm)
    nsegm <- length(segm[[1]])
            
    res <- list()
    SSR <- matrix(nrow = nsegm, ncol = ncomp + 1)
    for(i in seq_len(nrep)) {
        
        if(print)
            cat("/ rep=", i, " ", sep = "") 
        
        zsegm <- segm[[i]]
        
        for(j in seq_len(nsegm)) {
            
            s <- sort(zsegm[[j]])
            ns <- length(s)
            
            if(print)
                cat("segm=", j, " ", sep = "")
            
            fm <- algo(X[-s, , drop = FALSE], ncomp = ncomp, ...)
            zT <- .projscor(fm, X[s, , drop = FALSE])
            
            SSR[j, ] <- xssr(X[s, , drop = FALSE], 
                                             zT, fm$P, fm$xmeans) / (ns * p)
            
            }
        
        res[[i]] <- data.frame(
            rep = rep(i, ncomp + 1),
            ncomp = zncomp,
            ssr = (colSums(SSR) / nsegm) * N
            )
        
        }
    
    if(print)
        cat("/ End.")
    cat("\n\n")
    
    z <- setDF(rbindlist(res))
    z$df <- rep(df, nrep)
    z$df.ssr <- rep(df.ssr, nrep)
    z$msep <- z$ssr / z$df.ssr
    res <- z
    
    opt <- numeric()
    for(i in seq_len(nrep)) {
        u <- z[z$rep == i, ]
        opt[i] <- u$ncomp[u$msep == min(u$msep)]
        }
    
    ##### Summary
    
    z <- dtaggregate(ssr ~ ncomp, data = res, FUN = mean)
    z$df <- df
    z$df.ssr <- df.ssr
    z$msep <- dtaggregate(msep ~ ncomp, data = res, FUN = mean)$msep
    s2 <- dtaggregate(msep ~ ncomp, data = res, FUN = var)$msep
    z$se <- sqrt(s2 / nrep)
    res.summ <- z
    
    list(res.summ = res.summ, res = res, opt = opt)

    }
