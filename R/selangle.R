selangle <- function(
    X, Y = NULL, ncomp = NULL, algo = NULL,
    B = 50, seed = NULL,
    angle = c("maxsub", "hot"),
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    print = TRUE, 
    ...
    ) {
    
    angle <- match.arg(angle)   
  
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq_len(ncomp)
    
    if(is.null(Y)) {
        if(is.null(algo))
            if(n < p)
                algo <- pca_eigenk
            else
                algo <- pca_eigen
            fm <- algo(X, ncomp = ncomp, ...)
            } 
    else {
        if(is.null(algo))
            algo <- pls_kernel
        fm <- algo(X, Y, ncomp = ncomp, ...)
        }
    
    set.seed(seed = seed)
    Q <- matrix(nrow = B, ncol = ncomp)
    for(i in seq_len(B)) {
        
        if(print)
            cat(i, "")
        
        s <- sample(seq_len(n), size = n, replace = TRUE)
        if(is.null(Y))
            zfm <- algo(X[s, ], ncomp = ncomp)
        else {
            Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
            zfm <- algo(X[s, ], Y[s, ], ncomp = ncomp)
            }

        zq <- numeric()
        for(a in seq_len(ncomp))
            zq[a] <- .corvec(fm$P[, seq_len(a), drop = FALSE],
                             zfm$P[, seq_len(a), drop = FALSE], type = angle)

        Q[i, ] <- zq
        
        }
    q <- matrixStats::colMeans2(Q, na.rm = TRUE)
    q[q > 1] <- 1

    set.seed(seed = NULL)

    if(print)
        cat("\n\n")
    
    ## Output in radiants: right angle = pi/2)
    ## Radiants are then standardized to [0, 1] by dividing by pi/2 
    q <- acos(q) / (pi / 2)
    
    q[q == 0] <- 1e-4
    
    if(plot) {
      
        if(is.null(ylab))
            ylab <- "Angle"

        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))
        
        .plot_scree(q, ylim = c(.9 * min(q), 1.01),
                    xlab = xlab, ylab = ylab)
        abline(h = 1, col = "grey")
        
        .plot_scree(log(q), ylim = c(1.1 * min(log(q)), .02), 
                    xlab = xlab, ylab = ylab, main = "log-scale")
        abline(h = 0, col = "grey")
        
        par(oldpar)

        }
    
    list(q = q)
    
    }


