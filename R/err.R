err <- function(fm, formula = ~ 1, nam = NULL, digits = 4) {
  
  y <- fm$y
  fit <- fm$fit

  f <- as.character(formula)[2]
    
  if(is.null(nam)) nam <- names(y)[ncol(y)]
  
  y$e <- as.numeric(y[, nam] != fit[, nam])
  
  y$nbpred <- rep(1, nrow(y))
  
  z <- dtaggregate(formula(paste("nbpred ~", f)), data = y, FUN = sum)
  z$e <- dtaggregate(formula(paste("e ~", f)), data = y, FUN = sum)$e
  z$errp <- z$e / z$nbpred
  
  znam <- "errp"
  z[, znam] <- round(z[, znam], digits = digits)
  
  z
  
  }

