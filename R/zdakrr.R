.dakrr <- function(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, kern = kpol, 
                 weights = NULL, ...){
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  
  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)
  
  colnam.Y <- colnames(Yr)
  if(is.null(colnam.Y)) colnam.Y <- "y1"

  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)
  
  # levels returns the sorted character level names 
  lev <- levels(Yr)      
  
  if(!is.null(Yu)) 
    Yu <- as.character(Yu) 
  else 
    Yu <- rep(NA, m)
  
  lambda <- sort(unique(lambda))
  nlambda <- length(lambda)
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(lev, nlambda * m)
    dummyfit <- NULL
    }
  ### END
  
  else {
    
    if(is.null(weights))
      weights <- rep(1 / n, n)
    else
      weights <- weights / sum(weights) 
    
    fm <- .krr(Xr, dummy(Yr), Xu, lambda = lambda, 
             unit = unit, kern = kern, weights = weights, ...)
    dummyfit <- fm$fit[, lev, drop = FALSE]
  
    z <- apply(dummyfit, FUN = .findmax, MARGIN = 1) 
    fit <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
  
    }

  y <- rep(Yu, nlambda)
  r <- as.numeric(y != fit)
  
  y <- data.frame(lambda = sort(rep(lambda, m)), unit = rep(unit, nlambda * m), 
                  rownum = rep(1:m, nlambda), rownam = rep(rownam.Xu, nlambda), 
                  y, stringsAsFactors = FALSE)
  
  fit <- data.frame(lambda = sort(rep(lambda, m)), unit = rep(unit, nlambda * m), 
                  rownum = rep(1:m, nlambda), rownam = rep(rownam.Xu, nlambda), 
                  fit, stringsAsFactors = FALSE)

  r <- data.frame(lambda = sort(rep(lambda, m)), unit = rep(unit, nlambda * m), 
                  rownum = rep(1:m, nlambda), rownam = rep(rownam.Xu, nlambda), 
                  r, stringsAsFactors = FALSE)
                  
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y

  list(y = y, fit = fit, r = r, dummyfit = fm$fit, ni = ni)
  
  }