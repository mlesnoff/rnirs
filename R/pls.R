pls <- function(Xr, Yr, Xu = NULL, ncomp, algo = NULL, ...) {

    Xr <- .matrix(Xr)
    n <- dim(Xr)[1]
    
    Y <- .matrix(Yr, row = FALSE, prefix.colnam = "y")

    if(is.null(algo))
        algo <- pls_kernel        

    fm <- algo(Xr, Y, ncomp, ...)
    
    tt <- fm$TT
    ## = Variances of the scores if T is centered
    tt.adj <- colSums(fm$P * fm$P) * tt
    ## = Adjustment required since P are not orthogonal
    
    Xr <- .center(Xr, fm$xmeans)
    xsstot <- sum(fm$weights * Xr * Xr, na.rm = TRUE)
    
    pvar <- tt.adj / xsstot
    cumpvar <- cumsum(pvar)
    xvar <- tt.adj / n            
    
    z <- data.frame(ncomp = seq_len(ncomp), var = xvar, pvar = pvar, cumpvar = cumpvar)
    row.names(z) <- seq_len(ncomp)
    explvarx <- z
    
    Tu <- NULL
    if(!is.null(Xu))
        Tu <- .projscor(fm, .matrix(Xu))
    
    list(Tr = fm$T, Tu = Tu, P = fm$P, W = fm$W, R = fm$R, C = fm$C, TT = fm$TT,
        xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights, 
        explvarx = explvarx, T.ortho = fm$T.ortho)        
    
    }

