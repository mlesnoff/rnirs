cvpca_ia <- function(
    X, ncomp, algo = NULL,
    segm,
    start = "nipals",
    tol = .Machine$double.eps^0.5, 
    maxit = 10000, 
    print = TRUE, 
    ...
    ) {

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    N <- n * p
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq(0, ncomp)
    
    nrep <- length(segm)
    nsegm <- length(segm[[1]])
            
    X0 <- .center(X, matrixStats::colMeans2(X)) ## Add "weight"
    ssr0 <- sum(X0 * X0) / N
    
    res <- list()
    SSR <- matrix(nrow = nsegm, ncol = ncomp + 1)
    niter <- matrix(nrow = nrep, ncol = nsegm)
    for(i in seq_len(nrep)) {
        
        if(print)
            cat("/ rep=", i, " ", sep = "") 
        
        zsegm <- segm[[i]]
        
        for(j in seq_len(nsegm)) {
            
            s <- sort(zsegm[[j]])
            ns <- length(s)
            
            if(print)
                cat("segm=", j, " ", sep = "")
            
            Xna <- replace(X, s, NA)
            
            fm <- ximputia(Xna, ncomp = ncomp,
                                     start = start,
                                     tol = tol, maxit = maxit, print = FALSE, ...)
            
            niter[i, j] <- fm$niter
            
            for(a in seq_len(ncomp)) {
                
                Fit <- xfit(fm$T[, seq_len(a), drop = FALSE], 
                                        fm$P[, seq_len(a), drop = FALSE], fm$xmeans)[s]
                
                E <- X[s] - Fit
                
                SSR[j, a + 1] <- sum(E * E)
                
                }
            
            SSR[j, ] <- SSR[j, ] / (ns * p)
            SSR[j, 1] <- ssr0
            
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
    
    row.names(niter) <- paste("rep", seq_len(nrep), sep = "")
    colnames(niter) <- paste("segm", seq_len(nsegm), sep = "")
    conv <- ifelse(maxit > 1 & niter == maxit, FALSE, TRUE) 
    
    z <- setDF(rbindlist(res))
    z$msep <- z$ssr / N
    res <- z
    
    opt <- numeric()
    for(i in seq_len(nrep)) {
        u <- z[z$rep == i, ]
        opt[i] <- u$ncomp[u$msep == min(u$msep)]
        }
    
    ##### Summary
    
    z <- dtaggregate(ssr ~ ncomp, data = res, FUN = mean)
    z$msep <- dtaggregate(msep ~ ncomp, data = res, FUN = mean)$msep
    s2 <- dtaggregate(msep ~ ncomp, data = res, FUN = var)$msep
    z$se <- sqrt(s2 / nrep)
    res.summ <- z
    
    opt.summ <- z$ncomp[z$msep == min(z$msep)]
    
    list(res.summ = res.summ, opt.summ = opt.summ, 
             res = res, opt = opt, niter = niter, conv = conv)

    }
