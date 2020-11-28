mse <- function(fm, formula = ~ 1, nam = NULL, digits = 3) {
  
  f <- as.character(formula)[2]
  
  dat <- fm$y
  
  if(is.null(nam)) 
    nam <- names(dat)[ncol(dat)] 
  
  dat$y <- dat[, nam]
  dat$fit <- fm$fit[, nam]

  dat$e <- dat$y - dat$fit
  dat$e2 <- dat$e^2
  
  dat$nbpred <- rep(1, nrow(dat))
  
  z <- dtaggregate(formula = formula(paste("nbpred ~", f)), data = dat, FUN = sum)

  e2 <- dtaggregate(formula = formula(paste("e2 ~", f)), data = dat, FUN = sum)$e2
  msep <- e2 / z$nbpred
  rmsep <- sqrt(msep)
  
  .var <- function(x) {n <- length(x) ; (n - 1) / n * var(x)}
  sep <- sqrt(dtaggregate(formula = formula(paste("e ~", f)), data = dat, FUN = .var)$e)
  
  b <- -dtaggregate(formula = formula(paste("e ~", f)), data = dat, FUN = mean)$e

  mad <- dtaggregate(formula = formula(paste("e ~", f)), data = dat, FUN = mad)$e
  
  ## R2
  .foo <- function(x) {sum((x - mean(x))^2)}
  y.e2 <- dtaggregate(formula = formula(paste("y ~", f)), data = dat, FUN = .foo)$y
  y.msep <- y.e2 / z$nbpred
  r2 <- 1 - msep / y.msep 
  
  ## Corr2
  datdt <- setDT(dat)
  rvar <- attr(terms(formula), "term.labels")
  rho <- suppressWarnings(datdt[, by = rvar, 
    list(rho = cor(eval(parse(text = "y")), eval(parse(text = "fit"))))])
  ## below, this generates a Note in Package Checking
  #rho <- suppressWarnings(datdt[, by = rvar, list(rho = cor(y, fit))])
  ## End
  rho <- rho$rho
  
  z$msep <- msep
  z$mad <- mad
  z$rmsep <- rmsep
  z$sep <- sep
  z$b <- b 
  z$r2 <- r2
  z$cor2 <- rho^2
  
  znam <- c("msep", "mad", "rmsep", "sep", "b","r2", "cor2")
  z[, znam] <- round(z[, znam], digits = digits)

  z
  
  }

