blocksopca <- function(Xr, Xu = NULL, ncomp, 
                       blocks, colblocks = NULL, ...) {
    ## If argument 'blocks = NULL',
    ## an object 'blocks' (= a list with the block indexes) is created
    if(!is.null(colblocks)) {
        lev <- levels(as.factor(colblocks))
        nlev <- length(lev)
        blocks <- vector(mode = "list", length = nlev)
        for(i in seq_len(nlev))
            blocks[[i]] <- which(colblocks == lev[i])    
        }

    nbl <- length(blocks)
    
    if(length(ncomp) == 1) 
        ncomp <- rep(ncomp, nbl)
    
    ## Case "sum(ncomp) = 0"
    if(sum(ncomp) == 0) {
    
        fm <- list()
        fm$Tu <- fm$Tr <- NA
        fm$blocks <- NA
        fm$ncomp <- ncomp
        
        }
    
    ## Other cases
    else {
        u <- which(ncomp > 0)
        fm <- .blocksopca(Xr = Xr, Xu = Xu, ncomp = ncomp[u], 
                          blocks = blocks[u], ...)
        }
    
    fm
        
    }