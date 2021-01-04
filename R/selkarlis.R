selkarlis <- function(
    X, ncomp, algo = NULL,
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    ...
    ) {
    
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
    
    zmean <- 1   ## = sum(eig) / p
    se <- sqrt(p - 1) / sqrt(n - 1)
    eig <- fm$eig
    eig.lo <- eig - 2 * se
    
    opt <- NA
    u <- which(eig.lo <= zmean)
    if(length(u) > 0)
        opt <-  min(u) - 1
  
    zncomp <- seq_len(ncomp)
    eig <- eig[zncomp]
    eig.lo <- eig.lo[zncomp]
    zmean <- rep(zmean, ncomp)
    zmean.up <- zmean + 2 * se
    
    if(plot) {
      
        #logeig.lo <- log(eig) - 1.96 * sqrt(  (p - 1) / (n - 1) * 1 / eig^2  )
      
        if(is.null(ylab))
            ylab <- "Eig."

        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))
        
        .plot_scree(eig, 
                    xlab = xlab, ylab = ylab)
        lines(zmean.up, col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], eig[u], pch = 16, col = "grey", cex = 1.2)
                }
        
        .plot_scree(log(eig), 
                    xlab = xlab, ylab = ylab, main = "log-scale")
        lines(log(zmean.up), col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], log(eig[u]), pch = 16, col = "grey", cex = 1.2)
                }
        
        par(oldpar)

        }
  
  
    list(opt = opt, eig = eig, eig.lo = eig.lo, zmean = zmean, 
         zmean.up = zmean.up)
    
    }


