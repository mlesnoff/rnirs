.kplsr <- function(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, 
                                 weights = NULL, ...) {
    
    Xr <- .matrix(Xr)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)    
    
    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
    q <- dim(Yr)[2]
    colnam.Y <- colnames(Yr)
    ymeans <- .xmean(Yr, weights)
    
    if(is.null(Yu)) 
        Yu <- matrix(nrow = m, ncol = q)
    else {
        if(q == 1)
            row <- FALSE 
        else 
            row <- TRUE
        Yu <- .matrix(Yu, row = row)
        }
    
    Ku <- kern(Xu, Xr, ...)
    tK <- t(kern(Xr, ...))
    Kuc <- t(t(Ku - colSums(weights * t(Ku))) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    
    fm <- kpls_nipals(Xr, Yr, ncomp, kern, weights, ...)
    Tu <- Kuc %*% fm$R

    beta <- t(fm$C)        

    Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
    r <- fit <- y <- array(dim = c(m, ncomp + 1, q))
    y[, 1, ] <- Yu
    fit[, 1, ] <- Ymeans
    
    for(a in seq_len(ncomp)) {
        
        y[, a + 1, ] <- Yu
        fit[, a + 1, ] <- Ymeans + Tu[, seq_len(a), drop = FALSE] %*% beta[seq_len(a), , drop = FALSE]
        
        }
    
    y <- matrix(c(y), nrow = m * (ncomp + 1), ncol = q, byrow = FALSE)
    fit <- matrix(c(fit), nrow = m * (ncomp + 1), ncol = q, byrow = FALSE)
    r <- y - fit

    dat <- data.frame(
        ncomp = sort(rep(seq(0, ncomp), m)),
        rownum = rep(seq_len(m), ncomp + 1),
        rownam = rep(rownam.Xu, ncomp + 1)
        )
    
    y <- cbind(dat, y)
    fit <- cbind(dat, fit)
    r <- cbind(dat, r)
    
    zq <- ncol(y)
    u <- seq(zq - q + 1, zq)
    names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Y
    
    list(y = y, fit = fit, r = r, 
        Tr = fm$T, Tu = Tu, C = fm$C, 
        weights = fm$weights, T.ortho = fm$T.ortho)

    }

