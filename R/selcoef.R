selcoef <- function(
    X, Y = NULL, ncomp = NULL, algo = NULL,
    B = 50, seed = NULL,
    alpha = .05, lim = .01,
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    print = TRUE, 
    ...
    ) {
    
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
        
        P[, , k] <- zfm$P
        #P[, , k] <- zfm$R    ## not good
        
        }

    if(print)
        cat("\n\n")
    
    Z <- matrix(nrow = p, ncol = ncomp)
    q <- numeric()
    for(a in seq_len(ncomp)) {
        
      if(print)
          cat(a, "")
      
      zP <- abs(P[, a, ])
      
      lo <- apply(zP, MARGIN = 1, FUN = quantile, probs = alpha / 2)
      z <- ifelse(lo <= lim, 1, 0)
      q[a] <- sum(z) / p
      
      }
    set.seed(seed = NULL)
    q[q == 0] <- 1e-4

    if(print)
        cat("\n\n")
    
    if(plot) {
      
        if(is.null(ylab))
            ylab <- "Prop. coef~0" #"Prop. non-meaningful"

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
    
    list(p = q)
    
    }


