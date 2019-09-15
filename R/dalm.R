dalm <- function(Xr, Yr, Xu, Yu = NULL, weights = NULL){
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  
  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)
  
  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"

  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)
  
  lev <- levels(Yr)
  namclas <- as.character(lev)
  
  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m)
  
  if(is.null(weights)) d <- rep(1 / n, n) else d <- weights / sum(weights)
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(namclas, m)
    dummyfit <- NULL
    }
  ### END
  
  else {
  
    zXr <- cbind(rep(1, n), Xr)
    zXu <- cbind(rep(1, m), Xu)
    Yrdummy <- dummy(Yr)
    
    zXr.d <- d * zXr 
    
    Beta <- solve(crossprod(zXr.d, zXr)) %*% crossprod(zXr.d, Yrdummy)
    
    z <- zXu %*% Beta
    
    row.names(z) <- rownam.Xu
    colnames(z) <- namclas
    dummyfit <- z
  
    # if ex-aequos, the first is selected
    z <- apply(dummyfit, FUN = function(x) which.max(x), MARGIN = 1) 
    fit <- sapply(z, FUN = function(x) namclas[x])
  
    }

  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = 1:m, rownam = rownam.Xu, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = 1:m, rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = 1:m, rownam = rownam.Xu, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr

  list(y = y, fit = fit, r = r, dummyfit = dummyfit, ni = ni)
  
  }