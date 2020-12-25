stackavg <- function(fit, y = NULL, formula = ~ 1, nam = NULL, weights = NULL) {
    
    m <- nrow(fit)
    
    f <- paste(" ~ ", as.character(formula)[2])

    if(is.null(nam)) nam <- names(fit)[ncol(fit)]
    
    if(is.null(weights)) w <- rep(1, m) else w <- weights
    
    fit$w <- w
    z <- dtaggregate(
        formula = formula(paste("w", f)), 
        data = fit, FUN = sum
        )
    names(z)[ncol(z)] <- "wtot"
    
    u <- merge(fit, z, by = colnames(z)[-ncol(z)])
    u$wfin <- u$w / u$wtot
    u[, nam]    <- u[, nam] * u$wfin
    fitw <- u
    
    z <- dtaggregate(
        formula = formula(paste(nam, f)), 
        data = fitw, FUN = sum
        )
    ## TMP FOR CHECKING
    #v$wtot <- dtaggregate(formula = formula(paste("wfin", f)), data = fitw, FUN = sum)$wfin
    ## END
    fit.avg <- z
    
    if(is.null(y)) {
        
        y.avg <- fit.avg
        y.avg[, nam] <- rep(NA, nrow(y.avg))
        
        }
    else
        # y (= data) is the same for all the models.
        # Therefore, a non-weighted mean is required here.
        y.avg <- dtaggregate(
            formula = formula(paste(nam, f)), 
            data = y, FUN = mean
            )
    
    r <- y.avg
    r[, nam] <- y.avg[, nam] - fit.avg[, nam] 
    
    list(y = y.avg, fit = fit.avg, r = r, fitw = fitw)

    }


