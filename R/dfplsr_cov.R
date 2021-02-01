dfplsr_cov <- function(
    X, Y, ncomp, algo = NULL,
    B = 50, seed = NULL, 
    print = TRUE, 
    ...
    ) {
    
    if(is.null(algo))
        algo <- pls_kernel
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    ncomp <- min(ncomp, p)

    fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo, ...)
    z <- mse(fm, ~ ncomp, digits = 25)
    ssr <- z$nbpred * z$msep
    ## Same as
    ## fm <- algo(X, Y, ncomp = ncomp, ...)
    ## ssr <- numeric()
    ## for(a in seq_len(ncomp))
    ##    ssr[a] <- sum(.resid.pls(fm, Y, ncomp = a)$r^2)
    ## End
    
    ## mu and s2 for the parametric bootstrap
    ## estimated from a low biased model
    k <- min(ncomp, 30)
    ## Below s2 is not the unbiased estimate of sigma2 for the model
    ## This unbiased estimate would need to know df ...
    ## This is not important here, since the amount put in 
    ## the simulated variations is counter-balanced by the covariances
    ## Efron 2004 p. 620 is not clear how he calculates s2
    ## "obtained from residuals of some 'big' model presumed 
    ## to have negligible bias"
    s2 <- ssr[k + 1] / (n - k - 1)
    zfm <- algo(X, Y, ncomp = k, ...)
    ## In Efron 2004, mu is estimated for each number of LV
    ## This is a simplification here: mu is computed only one time 
    ## from the low-biased model
    mu <- .resid.pls(zfm, Y, ncomp = k)$fit
        
    zY <- matrix(rep(mu, B), nrow = n, ncol = B, byrow = FALSE)
    set.seed(seed = seed)
    zE <- matrix(rnorm(n * B, sd = s2^.5), nrow = n, ncol = B)
    set.seed(seed = NULL)
    zY <- zY + zE
    
    Fit <- array(dim = c(n, B, ncomp)) 
    for(j in seq_len(B)) {
        
        if(print)
            cat(j, " ")

        z <- plsr(X, zY[, j], X, zY[, j], ncomp = ncomp, algo = algo, ...)$fit
        zfit <- z[z$ncomp >= 1, ]
        
        Fit[, j, ] <- zfit[, ncol(zfit)]

        }
    
    if(print)
        cat("\n\n")
    
    Cov <- matrix(nrow = n, ncol = ncomp)
    for(a in seq_len(ncomp))
        for(i in seq_len(n))
            Cov[i, a] <- cov(zY[i, ], Fit[i, , a])
        
    cov <- colSums(Cov)
    df <- c(1, cov / s2)
    
    list(df = df)
    
    }


