selhorn <- function(X, ncomp, algo = NULL,
                    nrep = 10,
                    plot = TRUE,
                    xlab = "Nb. components", ylab = NULL,
                    print = TRUE,
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
    fm <- algo(X, ncomp = 1, ...)
    eig <- fm$eig
    
    res <- matrix(nrow = nrep, ncol = length(eig))
    for(i in seq_len(nrep)) {
        
        if(print)
            cat(i, " ")
      
        res[i, ] <- algo(
            matrix(rnorm(n * p), nrow = n, ncol = p), 
            ncomp = 1
            )$eig
        
        }
    if(print)
        cat("\n\n")
    zmean <- matrixStats::colMeans2(res)

    u <- which(eig <= zmean)
    if(length(u) > 0)
        opt <-  min(u) - 1
    
    zncomp <- seq_len(ncomp)
    eig <- eig[zncomp]
    zmean <- zmean[zncomp]
    
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


