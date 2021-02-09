aicplsr <- function(
    X, Y, ncomp, algo = NULL,
    methdf = c("div", "cov", "crude"),
    theta = 3,
    correct = TRUE,
    B = 50,
    print = TRUE, 
    ...
    ) {
    
    methdf <- match.arg(methdf) 
    if(is.null(algo))
        algo <- pls_kernel

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    ncomp <- min(ncomp, n, p)
    
    fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo)
    z <- mse(fm, ~ ncomp, digits = 25)
    ssr <- z$nbpred * z$msep

    if(methdf == "div") 
        df <- dfplsr_div(
            X, Y, ncomp = ncomp, algo = algo, 
            B = B, 
            print = print, 
            ...
            )$df    
    if(methdf == "cov") 
        df <- dfplsr_cov(
            X, Y, ncomp = ncomp, algo = algo,
            B = B, 
            print = print, 
            ...
            )$df
    if (methdf == "crude") 
        df <- 1 + theta * seq(0, ncomp)
    
    df.ssr <- n - df
    
    ## For Cp, unbiased estimate of sigma2 
    ## ----- Cp1: From a low biased model
    ## Not stable with dfcov and ncomp too large compared to best model !!
    ## If df stays below .95 * n, this corresponds
    ## to the maximal model (ncomp)
    ## Option 2 gives in general results
    ## very close to those of option 1,
    ## but can give poor results with dfcov
    ## when ncomp is set too large to the best model
    k <- max(which(df <= .50 * n))
    s2.1 <- ssr[k] / df.ssr[k]
    ## ----- Cp2: FPE-like
    ## s2 is estimated from the model under evaluation
    ## Used in Kraemer & Sugiyama 2011 Eq.5-6
    s2.2 <- ssr / df.ssr
    ## Option 3: From a low biased model (alternative)
    ## The following option can generate poor Cp estimates 
    ## if ncomp is over-large for small data
    ## ==> not used
    #if(option == 3) {
    #    minlv <- 15
    #    k <- min(minlv, ncomp)
    #    u <- (k + 1):(ncomp + 1)
    #    s2 <- median(ssr[u] / (n - df[u]))
    #    }
    ## End
    
    ct <- rep(1, ncomp + 1)
    if(correct) 
        ct <- n / (n - df - 2)
    ct[df > n | ct <= 0] <- NA 
    ## For safe predictions when df stabilizes 
    ## and fluctuates
    ct[df > .80 * n] <- NA
    ## End
    u <- which(is.na(ct))
    if(length(u) > 0)
        ct[seq(min(u), ncomp + 1)] <- NA
    
    aic <- n * log(ssr) + 2 * (df + 1) * ct
    cp1 <- ssr + 2 * s2.1 * df * ct
    cp2 <- ssr + 2 * s2.2 * df * ct
    #fpe <- ssr * (n + df) / (n - df) * ct
    
    cp1 <- cp1 / n
    cp2 <- cp2 / n
    
    crit <- data.frame(aic = aic, cp1 = cp1, cp2 = cp2)
    delta <- data.frame(
        apply(crit, MARGIN = 2, FUN = function(x) x - min(x, na.rm = TRUE))
        )
    opt <- apply(
        crit, MARGIN = 2,
        FUN = function(x) which(x == min(x, na.rm = TRUE))[1] - 1
        )

    crit <- data.frame(
        ncomp = seq(0, ncomp), n = rep(n, ncomp + 1),
        df = df, ct = ct,
        ssr = ssr, 
        crit
        )
    
    list(crit = crit, delta = delta, opt = opt)
    
    }
