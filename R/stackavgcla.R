stackavgcla <- function(fit, y = NULL, formula = ~ 1, nam = NULL, weights = NULL) {
    
    m <- nrow(fit)
    
    if(is.null(nam)) nam <- names(fit)[ncol(fit)]
    
    f <- paste(" ~ ", as.character(formula)[2])
    fy <- paste(f, "+", nam)

    if(is.null(weights)) w <- rep(1, m) else w <- weights
    
    ###### fit
    
    fit[, nam] <- gsub(" ", "", fit[, nam])
    fit[, nam] <- gsub("[()]", "", fit[, nam])
    
    z <- dummy(fit[, nam])
    lev <- colnames(z)
    dum.colnam <- paste("dumm.", lev, sep = "")
    colnames(z) <- dum.colnam
    ndum <- ncol(z)
    
    fit <- cbind(fit, z)
    fit$w <- w
    
    z <- dtaggregate(
        formula = formula(paste("w", f)), 
        data = fit, FUN = sum
        )
    names(z)[ncol(z)] <- "wtot"

    u <- merge(fit, z, by = colnames(z)[-ncol(z)])
    u$wfin <- u$w / u$wtot
    u[, dum.colnam] <- u$wfin * u[, dum.colnam]
    fitw <- u
    
    for(i in seq_len(ndum)) {
        
        zf <- formula(paste(dum.colnam[i], f))
        z <- dtaggregate(formula = zf, data = fitw, FUN = sum)
        
        if(i == 1) res <- z else res <- cbind(res, z[, ncol(z)])
        names(res)[ncol(res)] <- dum.colnam[i]
        
        }
    
    z <- apply(res[, dum.colnam], FUN = .findmax, MARGIN = 1) 
    z <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
    
    res <- data.frame(res, z, stringsAsFactors = FALSE)
    names(res)[ncol(res)] <- nam
    
    fit.avg <- res
    
    ###### y
    
    if(is.null(y)) {
        
        y.avg <- fit.avg
        y.avg[, nam] <- rep(NA, nrow(y.avg))
        
        }
    else {
    
        y[, nam] <- gsub(" ", "", y[, nam])
        y[, nam] <- gsub("[()]", "", y[, nam])

        z <- dummy(y[, nam])
        lev <- colnames(z)
        dum.colnam <- paste("dumm.", lev, sep = "")
        colnames(z) <- dum.colnam
        ndum <- ncol(z)
    
        y <- cbind(y, z)
        
        for(i in seq_len(ndum)) {
            zf <- formula(paste(dum.colnam[i], f))
            z <- dtaggregate(formula = zf, data = y, FUN = sum)
            
            if(i == 1) res <- z else res <- cbind(res, z[, ncol(z)])
            names(res)[ncol(res)] <- dum.colnam[i]
            }
        
        z <- apply(res[, dum.colnam], FUN = .findmax, MARGIN = 1) 
        z <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
        
        res <- data.frame(res, z, stringsAsFactors = FALSE)
        names(res)[ncol(res)] <- nam
        
        y.avg <- res

    }
    
    r <- y.avg
    r[, nam] <- as.numeric(y.avg[, nam] != fit.avg[, nam])
    
    ###### end    
    
    list(y = y.avg, fit = fit.avg, r = r, fitw = fitw)

    }
