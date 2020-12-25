lmr <- function(Xr, Yr, Xu, Yu = NULL, weights = NULL) {
    
    Xr <- .matrix(Xr)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)

    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
    q <- ncol(Yr)
    colnam.Yu <- colnames(Yr)
    
    if(is.null(Yu)) Yu <- matrix(nrow = m, ncol = q)
        else Yu <- .matrix(Yu, row = FALSE, prefix.colnam = "y")
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights) 
    
    fm <- lm(Yr ~ Xr, weights = weights)
    
    y <- Yu
    fit <- cbind(rep(1, m), Xu) %*% fm$coef
    r <- y - fit
    
    dat <- data.frame(
        rownum = seq_len(m), 
        rownam = rownam.Xu
        )
    
    y <- cbind(dat, y)
    fit <- cbind(dat, fit)
    r <- cbind(dat, r)
    
    zq <- dim(y)[2]
    u <- seq(zq - q + 1, zq)
    names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Yu
    
    list(y = y, fit = fit, r = r, fm = fm)
    
    }