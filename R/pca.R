pca <- function(Xr, Xu = NULL, ncomp, algo = NULL, ...) {
    
    Xr <- .matrix(Xr)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    ncomp <- min(ncomp, n, p)
    zncomp <- seq_len(ncomp)
    
    if(is.null(algo))
        if(n < p)
            algo <- pca_eigenk
        else
            algo <- pca_eigen
    
    fm <- algo(Xr, ncomp, ...)

    zTT <- fm$weights * fm$T * fm$T
    tt <- colSums(zTT)
    ## = Variances of the scores if T is centered
    ## = eig except for pca_cr and pca_sph
    
    Xr <- .center(Xr, fm$xmeans)
    xsstot <- sum(fm$weights * Xr * Xr, na.rm = TRUE)
    ## = sum of the variances of the columns
    ## = trace of Cov(Xr)
    ## = sum(diag(crossprod(fm$weights * Xr, Xr)))
    
    pvar <- tt / xsstot
    cumpvar <- cumsum(pvar)
    
    ## Weighted SSR (not usefull here)
    #ssr <- n * (xsstot - cumsum(tt))
    
    z <- data.frame(ncomp = zncomp, var = tt, pvar = pvar, cumpvar = cumpvar)
    row.names(z) <- zncomp
    explvar <- z
    
    contr.ind <- .scale(zTT, center = rep(0, ncomp), tt)
    
    xvars <- .xvar(Xr, fm$weights)
    zX <- .scale(Xr, rep(0, p), sqrt(xvars))    
    zT <- .scale(fm$T, rep(0, ncomp), sqrt(tt))
    cor.circle <- t(fm$weights * zX) %*% zT
    
    coord.var <- crossprod(
        Xr, 
        fm$weights * .scale(fm$T,    rep(0, ncomp), sqrt(tt))
        )

    z <- coord.var^2
    contr.var <- .scale(z, rep(0, ncomp), colSums(z))
    
    row.names(contr.ind) <- row.names(Xr)
    
    contr.ind <- data.frame(contr.ind)
    coord.var <- data.frame(coord.var)
    contr.var <- data.frame(contr.var)
    cor.circle <- data.frame(cor.circle)
    
    Tu <- NULL
    if(!is.null(Xu))
        Tu <- .projscor(fm, .matrix(Xu))
    
    list(Tr = fm$T, Tu = Tu, P = fm$P, R = fm$R, eig = fm$eig,
        xmeans = fm$xmeans, weights = fm$weights, 
        explvar = explvar, contr.ind = contr.ind, coord.var = coord.var, 
        contr.var = contr.var, cor.circle = cor.circle, T.ortho = fm$T.ortho) 
    
    }

