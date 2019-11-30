stackavg <- function(fit, y = NULL, formula = ~ 1, nam = NULL, weights = NULL) {
  
  m <- nrow(fit)
  
  f <- paste(" ~ ", as.character(formula)[2])

  if(is.null(nam)) nam <- names(fit)[ncol(fit)]
  
  w <- weights
  if(is.null(weights)) w <- rep(1, m)
  
  fit$w <- w
  zf <- paste("w", f)
  z <- dtaggregate(formula = formula(zf), data = fit, FUN = sum)
  names(z)[ncol(z)] <- "wtot"
  
  u <- merge(fit, z, by = colnames(z)[-ncol(z)])
  u$wfin <- u$w / u$wtot
  u[, nam]  <- u[, nam] * u$wfin
  fitw <- u
  
  zf <- paste(nam, f)
  z <- dtaggregate(formula = formula(zf), data = fitw, FUN = sum)
  ## TMP FOR CHECKING
  #v$wtot <- dtaggregate(formula = formula(paste("wfin", f)), data = fitw, FUN = sum)$wfin
  ## END
  zfit <- z
  
  if(is.null(y)) {
    zy <- zfit
    zy[, nam] <- rep(NA, nrow(zy))
    }
  else
    zy <- dtaggregate(formula = formula(zf), data = y, FUN = mean)
  
  r <- zy
  r[, nam] <- zy[, nam] - zfit[, nam] 
  
  list(y = zy, fit = zfit, r = r, fitw = fitw)

  }


