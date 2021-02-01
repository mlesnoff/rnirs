cpplsr <- function(
    X, Y, ncomp, algo = NULL,
    type = c("aicc", "aic"),
    methdf = c("cov", "div", "crude"),
    theta = 3, 
    maxlv = 15,
    B = 50,
    eps = 1e-4,
    seed = NULL,
    print = TRUE, 
    ...
    ) {
    
    type <- match.arg(type) 
    methdf <- match.arg(methdf) 
    
    if(is.null(algo))
        algo <- pls_kernel

    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo, ...)
    z <- mse(fm, ~ ncomp, digits = 25)
    ssr <- z$nbpred * z$msep
    
    if(methdf == "cov") 
        df <- dfplsr_cov(X, Y, ncomp = ncomp, algo = algo, 
                         B = B, seed = seed, print = print, ...)$df
    
    if(methdf == "div") 
        df <- dfplsr_div(X, Y, ncomp = ncomp, algo = algo, 
                         ns = B, eps = eps, seed = seed, print = print, ...)$df
    
    if (methdf == "crude") 
        df <- 1 + theta * seq(0, ncomp)
    
    df.ssr <- n - df
    ## Unbiased estimate of sigma2 for the low biased model
    k <- min(ncomp, maxlv)
    s2 <- ssr[k + 1] / df.ssr[k + 1]

    r <- switch(
        type,
        aicc = ssr + 2 * s2 * df * n / (n - df - 1),
        aic = ssr + 2 * s2 * df,
        #bic = ssr + log(n) * s2 * df
        ##fpe = (n + df) / (n - df) * ssr,
        ##gcv = n / (df.ssr)^2 * ssr
        )

    ## Warning
    ## Here Cp is scaled by observation
    r <- r / n
    
    delta <- r - min(r)
    z <- exp(-.5 * delta)
    w <- z / sum(z)

    opt <- which(r == min(r))[1] - 1

    res <- data.frame(
        ncomp = seq(0, ncomp), n = rep(n, ncomp + 1),
        ssr = ssr, df = df, df.ssr = df.ssr,
        crit = r, delta = delta, w = w
        )
    
    list(res = res, opt = opt, k = k, s2 = s2)
    
    
    }
