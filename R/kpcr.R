kpcr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, 
                                 kern = kpol, weights = NULL, print = TRUE, ...) { 
    
    if(is.character(kern)) {
        namkern <- kern
        kern <- get(kern)
        }
    else
        namkern <- as.character(substitute(kern))
    
    dots <- list(...)
    namdots <- names(dots)
    ndots <- length(dots)
    
    z <- formals(kern)
    nam <- names(z)
    nam <- nam[-match(c("X", "Y"), nam)]
    z <- z[nam]
    if(ndots > 0)
        for(i in seq_len(ndots))
            if(namdots[i] %in% nam)
                z[[namdots[i]]] <- dots[[i]]
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
            .kplsr,
            c(list(Xr = Xr, Yr = Yr, Xu = Xu, Yu = Yu, 
                         ncomp = ncomp, kern = kern, weights = weights), 
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

        
        
        
