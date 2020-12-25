.krr <- function(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, kern = kpol, 
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
    
    if(is.null(Yu)) 
        Yu <- matrix(nrow = m, ncol = q)
    else {
        if(q == 1)
            row <- FALSE 
        else 
            row <- TRUE
        Yu <- .matrix(Yu, row = row)
        }
    
    K <- kern(Xr, ...)
    tK <- t(K)
    Kc <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    Kd <- sqrt(weights) * t(sqrt(weights) * t(Kc))
    
    Ku <- kern(Xu, Xr, ...)
    Kuc <- t(t(Ku - colSums(weights * t(Ku))) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    
    fm <- eigen(Kd)
    tol <- sqrt(.Machine$double.eps)
    posit <- fm$values > max(tol * fm$values[1L], 0)
    
    A <- fm$vectors[, posit, drop = FALSE]
    eig <- fm$values[posit]
    sv <- sqrt(eig)
    
    Pr <- sqrt(weights) * .scale(A, scale = sv)
    Tr <- Kc %*% Pr    
    Tu <- Kuc %*% Pr    
        
    tTDY <- crossprod(Tr, weights * Yr)
    
    lambda <- sort(unique(lambda))
    nlambda <- length(lambda)
    zlambda <- unit * lambda
    
    ymeans <- .xmean(Yr, weights)
    Ymeans <- matrix(rep(ymeans, m), nrow = m, byrow = TRUE)
    r <- fit <- y <- array(dim = c(m, nlambda, q))
    tr <- vector(length = nlambda)

    for(i in seq_len(nlambda)) {
        
        z <- 1 / (eig + zlambda[[i]] / n)
        tr[i] <- sum(eig * z)
    
        beta <- z * tTDY

        y[, i, ] <- Yu
        fit[, i, ] <- Ymeans + Tu %*% beta

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

    list(y = y, fit = fit, r = r, tr = tr)

    }

