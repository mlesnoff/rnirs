blocksopls <- function(Xr, Yr, Xu = NULL, ncomp, 
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
  
  ## Case 'sum(ncomp) = 0'
  if(sum(ncomp) == 0) {
  
    fm <- list()
    fm$Tu <- fm$Tr <- NA
    
    Xr <- .matrix(Xr)
    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")   
    n <- dim(Xr)[1]
    dots <- list(...)
    ymeans <- .xmean(Yr, weights = list(...)$weights)
    fm$Fitr <- matrix(rep(ymeans, n), nrow = n, byrow = TRUE)
    
    if(!is.null(Xu)) {
      Xu <- .matrix(Xu)
      m <- dim(Xu)[1]
      fm$Fitu <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
      }
    else
      fm$Fitu <- NA
    
    fm$blocks <- NA
    fm$ncomp <- ncomp
    
    }
  
  ## Other cases
  else {
    u <- which(ncomp > 0)
    fm <- .blocksopls(Xr = Xr, Yr = Yr, Xu = Xu, ncomp = ncomp[u], 
                      blocks = blocks[u], ...)
    }
  
  fm
    
  }