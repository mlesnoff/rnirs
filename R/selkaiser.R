selkaiser <- function(
    X, ncomp, algo = NULL,
    ci = FALSE, alpha = 1,
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    ...
    ) {
    
    X <- .matrix(X)
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

    zmean <- sum(fm$eig) / p
    ## Cov(X) has p eigs
    ## If X scaled, Cov(X) = Cor(X), sum(eigs) = p
    
    eig.lo <- eig <- fm$eig
    if(ci)
        eig.lo <- eig.lo * exp(-2 * sqrt(2 / n))  ## = lower limit

    opt <- NA
    u <- which(eig.lo <= alpha * zmean)
    if(length(u) > 0)
        opt <-  min(u) - 1
    ## Jollife modification
    ## in Cangelosi & Goriely 2007 Section A4
    ## alpha = .7
    
    zncomp <- seq_len(ncomp)
    eig <- eig[zncomp]
    eig.lo <- eig.lo[zncomp]
    zmean <- rep(zmean, ncomp)

    if(plot) {
      
        if(is.null(ylab))
            ylab <- "Eig."

        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))
        
        .plot_scree(eig, 
                    xlab = xlab, ylab = ylab)
        if(ci)
            lines(eig.lo, lty = 2)
        lines(zmean, col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], eig[u], pch = 16, col = "grey", cex = 1.2)
                }
        
        .plot_scree(log(eig), 
                    xlab = xlab, ylab = ylab, main = "log-scale")
        if(ci)
            lines(log(eig.lo), lty = 2)
        lines(log(zmean), col = "red", lty = 2)
        if(!is.na(opt))
            if(opt < ncomp) {
                u <- seq(opt + 1, ncomp)
                points(zncomp[u], log(eig[u]), pch = 16, col = "grey", cex = 1.2)
                }
        
        par(oldpar)

        }
  
  
    list(opt = opt, eig = eig, eig.lo = eig.lo, zmean = zmean)
    
    }


