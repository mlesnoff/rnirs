pca_rob <- function(
    X, ncomp, 
    nsim = 1500, p.rm = .30, 
    step2 = TRUE, weights = NULL, 
    robust = TRUE, alpha = 0.01
    ) {
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    ncomp <- min(ncomp, n, p)
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)    
    
    if(n < p)
        algo <- pca_eigenk
    else
        algo <- pca_eigen
    
    r <- outstah(X, nsim = nsim)
    w1 <- .talworth(r, quantile(r, 1 - p.rm))

    r <- outeucl(X)
    w2 <- .talworth(r, quantile(r, 1 - p.rm))
    
    w <- w1 * w2

    fm <- algo(X, ncomp = ncomp, weights = weights * w)
    
    if(step2) {
        
        r <- outsdod(fm, X, robust = robust, alpha = alpha)
        
        w <- .talworth(r, 1)
        
        fm <- algo(X, ncomp = ncomp, weights = weights * w)
        
        }
    
    fm

    }
