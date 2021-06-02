.blocksopls <- function(
    Xr, Yr, Xu = NULL, ncomp, 
    blocks, colblocks = NULL, 
    ...
    ) {
    
    ## All components of the input 'ncomp' must be > 0
    
    ## If argument 'blocks' = NULL,
    ## an object 'blocks' is created (= a list with the block indexes)
    if(!is.null(colblocks)) {
        lev <- levels(as.factor(colblocks))
        nlev <- length(lev)
        blocks <- vector(mode = "list", length = nlev)
        for(i in seq_len(nlev))
            blocks[[i]] <- which(colblocks == lev[i])    
        }
    
    nbl <- length(blocks)
    
    Xr <- .matrix(Xr)
    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")     
    n <- dim(Xr)[1]
    q <- dim(Yr)[2]
    
    if(!is.null(Xu)) {
        Xu <- blocksel(.matrix(Xu), blocks)$X
        nullXu <- FALSE
        }
    ## Trick when Xu is NULL
    else {
        Xu <- Xr[1, , drop = FALSE]
        nullXu <- TRUE
        }
    m <- dim(Xu)[1]
    
    if(length(ncomp) == 1) 
        ncomp <- rep(ncomp, nbl)
    
    ## Used for defining the block indexes of the calculated scores
    zblocks <- data.frame(
        numcol = seq_len(sum(ncomp)), 
        bl = rep(seq_len(nbl), ncomp)
        ) 
        
    ## Reorganisation of the data based on the blocks defined in argument 'blocks'
    newdat <- blocksel(Xr, blocks)
    Xr <- newdat$X
    newblocks <- newdat$blocks
        
    ## First block
    fm <- pls(
        Xr[, newblocks[[1]], drop = FALSE],
        Yr,
        Xu[, newblocks[[1]], drop = FALSE],
        ncomp = ncomp[1], ...
        )
    Tr <- fm$Tr
    Tu <- fm$Tu
    Ymeansr <- matrix(rep(fm$ymeans, n), nrow = n, byrow = TRUE)
    Ymeansu <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
    beta <- t(fm$C)
    Fitr <- Ymeansr + fm$Tr %*% beta
    Fitu <- Ymeansu + fm$Tu %*% beta
    ## Block indexes for the scores
    blocks[[1]] <- zblocks$numcol[zblocks$bl == 1]
        
    ## Other blocks
    if(nbl > 1) {
        
        for(i in seq(2, nbl)) {
            
            z <- orthog(Tr, Xr[, newblocks[[i]], drop = FALSE], fm$weights)
                
            fm <- pls(
                Xr = z$Y,
                Yr = Yr - Fitr,
                Xu = Xu[, newblocks[[i]], drop = FALSE] - Tu %*% z$b,
                ncomp = ncomp[i], ...
                )
                
            Tr <- cbind(Tr, fm$Tr)
            Tu <- cbind(Tu, fm$Tu)
                    
            Ymeansr <- matrix(rep(fm$ymeans, n), nrow = n, byrow = TRUE)
            Ymeansu <- matrix(rep(fm$ymeans, m), nrow = m, byrow = TRUE)
            beta <- t(fm$C)
            Fitr <- Fitr + Ymeansr + fm$Tr %*% beta
            Fitu <- Fitu + Ymeansu + fm$Tu %*% beta
            
            ## Block indexes for the scores
            blocks[[i]] <- zblocks$numcol[zblocks$bl == i]
                    
            }

        }
    
    if(nullXu) 
        Fitu <- Tu <- NA
    
    list(Tr = Tr, Tu = Tu, 
         Fitr = Fitr, Fitu = Fitu, 
         blocks = blocks, ncomp = ncomp)    

    }
