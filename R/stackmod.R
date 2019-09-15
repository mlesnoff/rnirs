stackmod <- function(fit, y = NULL, formula = ~ 1, nam = NULL, weights = NULL) {
  
  m <- nrow(fit)
  
  f <- paste(" ~ ", as.character(formula)[2])

  if(is.null(nam)) nam <- names(fit)[ncol(fit)]
  
  if(is.null(weights)) w <- rep(1, m) else w <- weights
  
  fit$w <- w
  zf <- paste("w", f)
  z <- dtaggregate(formula = formula(zf), data = fit, FUN = sum)
  names(z)[ncol(z)] <- "wtot"
  
  u <- merge(fit, z, by = colnames(z)[-ncol(z)])
  
  u$wfin <- u$w / u$wtot
  u[, nam]  <- u[, nam] * u$wfin
  fitw <- u
  
  if(length(nam) == 1)
    zf <- paste(nam, f)
  else
    zf <- paste("cbind(", paste(nam, collapse = ","), ")", f)
  
  v <- aggregate(formula = formula(zf), data = u, FUN = sum)
  ## TMP FOR CHECKING
  #v$wtot <- aggregate(formula = formula(paste("wfin", f)), data = u, FUN = sum)$wfin
  ## END
  fit <- v

  if(!is.null(y))
    y <- aggregate(formula = formula(zf), data = y, FUN = mean)
  else {
    y <- fit
    y[, nam] <- NA
    }
    
  r <- y
  r[, nam] <- y[, nam] - fit[, nam] 
  
  list(y = y, fit = fit, r = r, fitw = fitw)

  }


