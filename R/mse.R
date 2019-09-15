mse <- function(fm, formula = ~ 1, nam = NULL, digits = 3) {
  
  y <- fm$y
  fit <- fm$fit

  f <- as.character(formula)[2]
  
  if(is.null(nam)) nam <- names(y)[ncol(y)] 
  
  y$e <- y[, nam] - fit[, nam]
  
  y$e2 <- y$e^2
  
  y$nbpred <- rep(1, nrow(y))
  
  z <- dtaggregate(formula = formula(paste("nbpred ~", f)), data = y, FUN = sum)

  e2 <- dtaggregate(formula = formula(paste("e2 ~", f)), data = y, FUN = sum)$e2
  msep <- e2 / z$nbpred
  rmsep <- sqrt(msep)
  
  .var <- function(x) {n <- length(x) ; (n - 1) / n * var(x)}
  sep2 <- dtaggregate(formula = formula(paste("e ~", f)), data = y, FUN = .var)$e
  sep <- sqrt(sep2)
  
  b <- -dtaggregate(formula = formula(paste("e ~", f)), data = y, FUN = mean)$e

  mad <- dtaggregate(formula = formula(paste("e ~", f)), data = y, FUN = mad)$e
  
  y$y <- y[, nam]
  .foo <- function(x) {sum((x - mean(x))^2)}
  y.e2 <- dtaggregate(formula = formula(paste("y ~", f)), data = y, FUN = .foo)$y
  y.msep <- y.e2 / z$nbpred
  
  ydt <- y
  ydt$y.dt <- y[, nam]
  ydt$fit.dt <- fit[, nam]
  ydt <- setDT(ydt)
  rvar <- attr(terms(formula), "term.labels")
  rho <- suppressWarnings(ydt[, by = rvar, 
    list(rho = cor(eval(parse(text = "y.dt")), eval(parse(text = "fit.dt"))))])
  # Generates a Note in Package Checking
  #rho <- suppressWarnings(ydt[, by = rvar, list(rho = cor(y.dt, fit.dt))])
  # End
  rho <- rho$rho
  
  z$mad <- mad
  z$rmsep <- rmsep
  z$sep <- sep
  z$b <- b 
  
  z$r2 <- 1 - msep / y.msep 
  
  z$cor2 <- rho^2
  
  znam <- c("mad", "rmsep", "sep", "b","r2", "cor2")
  z[, znam] <- round(z[, znam], digits = digits)

  z
  
  }

