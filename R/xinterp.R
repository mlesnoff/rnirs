xinterp <- function(X, w, meth = "cubic", ...) {
    
    X <- .matrix(X)
    p <- dim(X)[p]
    
    colnam <- suppressWarnings(as.numeric(colnames(X)))
    if(sum(is.na(colnam)) > 0) 
        colnam <- seq_len(p)
    w0 <- colnam
    
    .fun <- function(x, w0, w, method, ...)
        interp1(w0, x, w, method, ...)
    zX <- t(apply(X, FUN = .fun, MARGIN = 1, w0 = w0, w = w, method = meth))
    colnames(zX) <- w
    
    zX
    
    }