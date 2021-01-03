selbroken <- function(X, ncomp, algo = NULL,
                      plot = TRUE, 
                      xlab = "Nb. components", ylab = NULL,
                      ...) {
    
    X <- .scale(X, scale = sqrt(.xvar(X, ...)))
    
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    if(is.null(algo))
        if(n < p)
            algo <- pca_eigenk
        else
            algo <- pca_eigen    
    
    ncomp <- min(ncomp, n, p)
    fm <- algo(X, ncomp = ncomp, ...)
    eig <- fm$eig
    ## Frontier 1976 uses pvar
    ## pvar = eig / sum(eig) = eig / p
    ## (except for pca_cr and pca_sph)
    
    S <- p 
    ## !! not min(n, p)
    z <- 1 / seq_len(S)
    u <- seq(S, 1)
    g <- 1 / S * cumsum(z[u])[u]
    g <- p * g[seq_len(min(n, p))]
    ## Last multiplication by p since the function 
    ## uses eig instead of pvar
    ## Therefore g finally equals to 
    ## cumsum(z[u])[u], as in Jackson 1993, and Bro & Smilde 2014
  
    opt <- NA
    u <- which(eig <= g)
    if(length(u) > 0)
        opt <-  min(u) - 1
    
    zncomp <- seq_len(ncomp)
    eig <- eig[zncomp]
    zmean <- g[zncomp]
    
    if(plot) {
      
        if(is.null(ylab))
            ylab <- "Eig."

        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))
        
        .plot_scree(eig, 
                    xlab = xlab, ylab = ylab)
        lines(zmean, col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], eig[u], pch = 16, col = "grey", cex = 1.2)
                }
        on.exit(par(oldpar))
        
        .plot_scree(log(eig), 
                    xlab = xlab, ylab = ylab, main = "log-scale")
        lines(log(zmean), col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], log(eig[u]), pch = 16, col = "grey", cex = 1.2)
                }
        on.exit(par(oldpar))
        
        par(oldpar)

        }
  
    list(opt = opt, eig = eig, zmean = zmean) 
    
    }


