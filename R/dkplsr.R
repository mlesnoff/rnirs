dkplsr <- function(
    Xr, Yr, Xu, Yu = NULL, ncomp, 
    kern = kpol, weights = NULL, 
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
    
    fm <- r <- fit <- y <- fm <- vector(mode = "list", length = npar)
    
    if(print)
        cat(paste("\n Kernel parameters: ", namkern, "\n", sep = ""))

    for(i in seq_len(nrow(kpar))) {
        
        zkpar <- kpar[i, , drop = FALSE]
        
        if(print)
            print(zkpar)
        
        res <- do.call(
            kgram, 
            c(list(Xr = Xr, Xu = Xu, kern = kern), 
                zkpar)
            )
        
        fm[[i]] <- plsr(res$Kr, Yr, res$Ku, Yu, ncomp, 
                                        algo = pls_kernel, weights = weights)
        
        z <- dim(fm[[i]]$y)[1] 
        dat <- data.frame(matrix(rep(unlist(kpar[i, ]), z), ncol = npar, byrow = TRUE))
        names(dat) <- names(kpar)
        
        y[[i]] <- cbind(dat, fm[[i]]$y)
        fit[[i]] <- cbind(dat, fm[[i]]$fit)
        r[[i]] <- cbind(dat, fm[[i]]$r)

        }
    
    y <- setDF(rbindlist(y))
    fit <- setDF(rbindlist(fit))
    r <- setDF(rbindlist(r))    
        
    list(y = y, fit = fit, r = r, fm = fm)

    }

        
        
        
