rr <- function(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, 
                                        weights = NULL) {
    
    Xr <- .matrix(Xr)
    xmeans <- .xmean(Xr, weights)
    Xr <- .center(Xr, xmeans)
    zdim <- dim(Xr)
    n <- zdim[1]
    p <- zdim[2]
    
    if(is.null(weights))
        weights <- rep(1 / n, n)
    else
        weights <- weights / sum(weights)    
    
    Xu <- .center(.matrix(Xu), xmeans)
    m <- dim(Xu)[1]
    rownam.Xu <- row.names(Xu)
    
    Yr <- .matrix(Yr, row = FALSE, prefix.colnam = "y")
    q <- dim(Yr)[2]
    colnam.Y <- colnames(Yr)
    ymeans <- .xmean(Yr, weights)
    Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
    
    if(is.null(Yu)) 
        Yu <- matrix(nrow = m, ncol = q)
    else {
        if(q == 1)
            row <- FALSE 
        else 
            row <- TRUE
        Yu <- .matrix(Yu, row = row)
        }
    
    tol <- sqrt(.Machine$double.eps) 
    if(n >= p) {
        
        fm <- eigen(crossprod(sqrt(weights) * Xr), symmetric = TRUE)
        posit <- fm$values > max(tol * fm$values[1L], 0)
        eig <- fm$values[posit]
        V <- fm$vectors[, posit, drop = FALSE]
        
        } 
    else {
        
        zX <- sqrt(weights) * Xr
        fm <- eigen(tcrossprod(zX), symmetric = TRUE)
        posit <- fm$values > max(tol * fm$values[1L], 0)
        eig <- fm$values[posit]
        U <- fm$vectors[, posit, drop = FALSE]
        V <- crossprod(zX, .scale(U, scale = sqrt(eig)))
        
        } 
    
    Tr <- Xr %*% V
    Tu <- Xu %*% V
        
    lambda <- sort(unique(lambda))
    nlambda <- length(lambda)
    zlambda <- unit * lambda
    
    r <- fit <- y <- array(dim = c(m, nlambda, q))
    tr <- vector(length = nlambda)
    b <- vector(length = nlambda, mode = "list")
    
    tTDY <- crossprod(Tr, weights * Yr)
    
    for(i in seq_len(nlambda)) {
        
        z <- 1 / (eig + zlambda[[i]] / n)
        tr[i] <- sum(eig * z)
    
        beta <- z * tTDY

        y[, i, ] <- Yu
        fit[, i, ] <- Ymeans + Tu %*% beta
        
        zb <- V %*% beta
        row.names(zb) <- colnames(Xr)
        int <- ymeans - crossprod(xmeans, zb)
        zb <- rbind(int, zb)
        row.names(zb)[1] <- "intercept"
        b[[i]] <- zb

        }
    
    y <- matrix(c(y), nrow = m * nlambda, ncol = q, byrow = FALSE)
    fit <- matrix(c(fit), nrow = m * nlambda, ncol = q, byrow = FALSE)
    r <- y - fit

    dat <- data.frame(
        lambda = sort(rep(lambda, m)),
        unit = rep(unit, nlambda * m),
        rownum = rep(seq_len(m), nlambda),
        rownam = rep(rownam.Xu, nlambda)
        )
    
    y <- cbind(dat, y)
    fit <- cbind(dat, fit)
    r <- cbind(dat, r)
    
    zq <- ncol(y)
    u <- seq(zq - q + 1, zq)
    names(r)[u] <- names(fit)[u] <- names(y)[u] <- colnam.Y

    list(y = y, fit = fit, r = r, b = b, tr = tr)

    }

