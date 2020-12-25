cppca <- function(X, ncomp, algo = NULL,
                                     segm = segm,
                                     type = c("aicc", "aic", "bic"), 
                                     k = 10,
                                     print = TRUE, ...) {
    
    X <- .matrix(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    N <- n * p    
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq(0, ncomp)
    
    fm <- pca(X, ncomp = ncomp, algo = algo, ...)
    
    ssr <- xssr(X, fm$Tr, fm$P, fm$xmeans)

    dfcal <- p    + (n - 1) * zncomp + p * zncomp - zncomp^2
    
    k <- min(k, ncomp)
    
    s2 <- ssr[k + 1] / (N - dfcal[k + 1])
    
    z <- cvpcarw(X, ncomp, algo = algo, 
                                segm = segm, print = print, ...)$res.summ

    R <- ssr / z$ssr
    R <- ifelse(R > 1, 1, R)
    
    df <- N * (1 - R)

    df.ssr <- N - df
    
    r <- switch(
        type,
        aic = ssr + 2 * s2 * df,
        aicc = ssr + 2 * s2 * df * N / (N - df - 1),
        bic = ssr + log(N) * s2 * df
        )
    
    r <- r / N
    
    delta <- r - min(r)
    z <- exp(-.5 * delta)
    w <- z / sum(z)
    
    opt <- which(r == min(r))[1] - 1

    res <- data.frame(
        ncomp = seq(0, ncomp), N = rep(N, ncomp + 1),
        ssr = ssr, ratio = R, df = df, df.ssr = df.ssr,
        crit = r, delta = delta, w = w
        )
    
    list(res = res, opt = opt, k = k, s2 = s2, ssr = ssr, df = dfcal)
    
    }
