svmc <- function(Xr, Yr, Xu, Yu = NULL,
                     C = 1, epsilon = .1, 
                     kern = c("poly", "rbf", "tanh"), print = TRUE, ...) {
  
  
  if(!is.factor(Yr))
    Yr <- as.factor(Yr)
  
  ni <- c(table(Yr))

  fm <- svmr(
    Xr, Yr, Xu, Yu,
    C, epsilon, 
    kern, 
    print, ...
    )
  
  z <- ncol(fm$y)
  fm$fit[, z] <- as.character(fm$fit[, z])
  if(!is.null(fm$r)) {
    fm$y[, z] <- as.character(fm$y[, z])
    fm$r <- fm$y
    fm$r[, z] <- as.numeric(fm$y[, z] != fm$fit[, z])
    }

  list(y = fm$y, fit = fm$fit, r = fm$r, ni = ni)
  
 }


