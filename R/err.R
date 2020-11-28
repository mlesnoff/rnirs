err <- function(fm, formula = ~ 1, nam = NULL, digits = 4) {
  
  f <- as.character(formula)[2]
    
  dat <- fm$y
  
  if(is.null(nam)) 
    nam <- names(dat)[ncol(dat)] 
  
  dat$y <- dat[, nam]
  dat$fit <- fm$fit[, nam]

  dat$e <- as.numeric(dat$y != dat$fit)
  
  dat$nbpred <- rep(1, nrow(dat))
  
  z <- dtaggregate(formula(paste("nbpred ~", f)), data = dat, FUN = sum)
  z$e <- dtaggregate(formula(paste("e ~", f)), data = dat, FUN = sum)$e
  z$errp <- z$e / z$nbpred
  
  znam <- "errp"
  z[, znam] <- round(z[, znam], digits = digits)
  
  z
  
  }

