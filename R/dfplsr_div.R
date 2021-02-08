dfplsr_div <- function(
    X, Y, ncomp, algo = NULL, 
    eps = 1e-2,
    samp = c("random", "syst"), 
    B = 30, seed = NULL,
    print = TRUE, 
    ...
    ) {
    
    samp <- match.arg(samp)
    
    if(is.null(algo))
        algo <- pls_kernel
   
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    ncomp <- min(ncomp, n, p)
    
    B <- min(B, n) 
    
    y <- c(Y)
    eps <- mean(y) * eps
    
    fm <- plsr(X, y, X, ncomp = ncomp, algo = algo, ...)
    fit <- fm$fit
    
    if(samp == "random") {
        set.seed(seed = seed)
        s <- sample(seq_len(n), size = B, replace = FALSE)
        set.seed(seed = NULL)
        }
    
    if(samp == "syst") {
        ## Regular sampling (grid) over y
        ## "order(y) = 3 7 etc." means: the 1st lower value is the component 3 of the vector y,
        ## the 2nd lower value is the component 7 of the vector y, etc.
        id <- order(y)
        u <- round(seq(1, n, length = B))
        s <- sort(id[u])
        }
    
    S <- matrix(nrow = B, ncol = ncomp)
    for(i in seq_len(B)) {
        
        if(print)
            cat(i, " ")
        
        zs <- s[i]
        
        zy <- y
        zy[zs] <- y[zs] + eps
        
        zfm <- plsr(X, zy, X[zs, , drop = FALSE], ncomp = ncomp, algo = algo, ...)
        zfit <- zfm$fit
        
        v <- numeric()
        for(a in seq_len(ncomp)) {
        
            fit.ref <- fit[fit$ncomp == a, ncol(fit)][zs]
            
            v[a] <- zfit[zfit$ncomp == a, ncol(zfit)] - fit.ref
            
            }
    
        S[i, ] <- v / eps
        
        }

    if(print)
        cat("\n\n")
    
    df <- colSums(S) * n / B
    df <- c(1, df)
    
    list(df = df)
    
    }


