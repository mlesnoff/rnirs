kplsda <- function(
    Xr, Yr, Xu, Yu = NULL, ncomp, 
    da = dalm, kern = kpol, 
    weights = NULL, 
    print = TRUE, 
    ...
    ) { 
    
    if(is.character(kern)) {
        namkern <- kern
        kern <- get(kern)
        }
    else
        namkern <- as.character(substitute(kern))
    
    dots <- list(...)
    namdots <- names(dots)
    
    z <- namdots[namdots %in% names(formals(da))]
    if (length(z) > 0) 
        dots.da <- dots[z]
    else dots.da <- NULL
    
    z <- namdots[namdots %in% names(formals(kern))]
    if (length(z) > 0) 
        dots.kern <- dots[z]
    else dots.kern <- NULL
    
    z <- formals(kern)
    nam <- names(z)
    nam <- nam[-match(c("X", "Y"), nam)]
    z <- z[nam]
    ndots <- length(dots.kern)
    if(ndots > 0)
        for(i in seq_len(ndots))
            if(names(dots.kern[i]) %in% nam)
                z[[names(dots.kern[i])]] <- dots.kern[[i]]
    listkpar <- lapply(z, FUN = function(x) sort(unique(x)))
    
    kpar <- expand.grid(listkpar)
    npar <- ncol(kpar)
    nampar <- names(kpar)
    
    r <- fit <- y <- vector(mode = "list", length = npar)
    
    if(print)
        cat(paste("\n Kernel parameters: ", namkern, "\n", sep = ""))

    for(i in seq_len(nrow(kpar))) {
        
        zkpar <- kpar[i, , drop = FALSE]
        
        if(print)
            print(zkpar)
        
        zfm <- do.call(
            .kplsda,
            c(list(Xr = Xr, Yr = Yr, Xu = Xu, Yu = Yu, 
                         ncomp = ncomp, da = da, kern = kern, weights = weights), dots.da,
                zkpar)
            )
        
        z <- dim(zfm$y)[1] 
        dat <- data.frame(matrix(rep(unlist(kpar[i, ]), z), ncol = npar, byrow = TRUE))
        names(dat) <- names(kpar)
        
        y[[i]] <- cbind(dat, zfm$y)
        fit[[i]] <- cbind(dat, zfm$fit)
        r[[i]] <- cbind(dat, zfm$r)        
        
        }
    
    y <- setDF(rbindlist(y))
    fit <- setDF(rbindlist(fit))
    r <- setDF(rbindlist(r))    
        
    list(y = y, fit = fit, r = r)

    }

        
        
        
