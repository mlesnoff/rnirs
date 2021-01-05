selcoll <- function(
    X, Y = NULL, ncomp = NULL, algo = NULL,
    B = 50, seed = NULL,
    type = c("loadings", "b"),
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    print = TRUE, 
    ...
    ) {
    
    type <- match.arg(type)
    
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
            }
    else {
        if(is.null(algo))
            algo <- pls_kernel
        }
    
    set.seed(seed = seed)
    P <- array(dim = c(p, ncomp, B))
    for(k in 1:B) {
        
        if(print)
            cat(k, "")
        
        s <- sample(seq_len(n), size = n, replace = TRUE)
        
        if(is.null(Y))
            zfm <- algo(X[s, ], ncomp = ncomp)
        else {
            Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
            zfm <- algo(X[s, ], Y[s, ], ncomp = ncomp)
            }
        
        ### b-coef
        if(!is.null(Y)) {
            if(type == "b" & dim(Y)[2] == 1)
                for(a in seq_len(ncomp))
                    zfm$P[, a] <- bcoef(zfm, ncomp = a)[-1]
            if(type == "b" & dim(Y)[2] > 1)
                stop("PLS2 not allowed for type = 'b'")
            }
         ### End
        
        P[, , k] <- zfm$P
        
        }

    q <- numeric()
    for(a in seq_len(ncomp)) {
        eig <- svd(P[, a, ], nu = 0, nv = 0)$d^2
        q[a] <- eig[1] / sum(eig)
        }

    set.seed(seed = NULL)

    if(print)
        cat("\n\n")
    
    r <- 1 - q
    r[r == 0] <- 1e-4

    
    if(plot) {
      
        if(is.null(ylab))
            ylab <- "Non-collinearity"

        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))
        
        .plot_scree(r, ylim = c(.9 * min(r), 1.01),
                    xlab = xlab, ylab = ylab)
        abline(h = 1, col = "grey")
        
        .plot_scree(log(r), ylim = c(1.1 * min(log(r)), .02), 
                    xlab = xlab, ylab = ylab, main = "log-scale")
        abline(h = 0, col = "grey")
        
        par(oldpar)

        }
    
    list(r = r)

    }


