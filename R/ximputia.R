ximputia <- function(
    X, ncomp, algo = NULL,
    start = c("nipals", "means"),
    tol = .Machine$double.eps^0.5, maxit = 10000,
    gs = TRUE,
    print = TRUE, 
    ...
    ) {

    start <- match.arg(start)
    
    dots <- list(...)
    namdot <- names(dots)
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    N <- n * p
    
    ncomp <- min(ncomp, n, p)
    
    if(is.null(algo))
        if(n < p)
            algo <- pca_eigenk
        else
            algo <- pca_eigen
    
    s <- which(is.na(X))
    if(length(s) == 0)
        stop("\n\nThere are no missing data in matrix X.\n\n")
    
    if(start == "nipals") {
        fm <- pca_nipalsna(X, ncomp, gs = gs)
        fit <- xfit(fm$T[, seq_len(ncomp), drop = FALSE], 
                                fm$P[, seq_len(ncomp), drop = FALSE], fm$xmeans)[s]
        }
    
    if(start == "means") {
        xmeans <- matrixStats::colMeans2(X, na.rm = TRUE)         ## to do: Add weight
        fit <- matrix(rep(xmeans, n), nrow = n, byrow = TRUE)[s]
        }

    X[s] <- fit
    
    iter <- 1 ; ztol <- 1
    while(ztol[iter] > tol & iter < maxit) {
        
        iter <- iter + 1
        if(print)
            cat(iter, " ") 
        
        fit <- X[s]
        
        fm <- algo(X, ncomp, ...)
        zfit <- xfit(fm$T[, seq_len(ncomp), drop = FALSE], 
                                 fm$P[, seq_len(ncomp), drop = FALSE], fm$xmeans)[s]
        X[s] <- zfit
        
        ztol[iter] <- .xnorm(fit - zfit) / .xnorm(fit)
        
        }
    
    if(print & iter > 1)
        cat("\n\n")
    
    if(maxit == 1)
        tol <- NA
    else
        tol <- ztol[-1]
    
    conv <- ifelse(maxit > 1 & iter == maxit, FALSE, TRUE) 
    
    list(X = X, T = fm$T, P = fm$P, xmeans = fm$xmeans, 
             fit = X[s], s = s, niter = iter, tol = tol, conv = conv) 

    }
