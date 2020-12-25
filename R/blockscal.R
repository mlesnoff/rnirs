blockscal <- function(Xr, Xu = NULL, blocks, colblocks = NULL, weights = NULL) {
    
    if(!is.null(colblocks)) {
        lev <- levels(as.factor(colblocks))
        nlev <- length(lev)
        blocks <- vector(mode = "list", length = nlev)
        for(i in seq_len(nlev))
            blocks[[i]] <- which(colblocks == lev[i])    
        }
    
    nbl <- length(blocks)
    
    n <- dim(Xr)[1]

    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)
    
    newdat <- blocksel(Xr, blocks)
    Xr <- newdat$X
    newblocks <- newdat$blocks
    
    if(!is.null(Xu))
        Xu <- blocksel(Xu, blocks)$X    
    
    xdisptot <- rep(NA, nbl)
    for(i in seq_len(nbl)) {
        
        z <- .xvar(Xr[, newblocks[[i]], drop = FALSE], weights = weights)
        
        xdisptot[i] <- sqrt(sum(z))
        
        Xr[, newblocks[[i]]] <- Xr[, newblocks[[i]], drop = FALSE] / xdisptot[i]
            
        }

    if(!is.null(Xu)) {
        
        Xu <- .matrix(Xu)
        
        for(i in seq_len(nbl))    
            Xu[, newblocks[[i]]] <- Xu[, newblocks[[i]], drop = FALSE] / xdisptot[i]
        
        }
        
    list(Xr = Xr, Xu = Xu, blocks = newblocks, xdisptot = xdisptot)    

    }