orthog <- function(X, Y, weights = rep(1, nrow(X))) {
  
  # Y is orthogonalized to X

  X <- .matrix(X)
  n <- nrow(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
  
  d <- weights / sum(weights)
  
  fm <- lm(Y ~ X, weights = d)
  
  b <- coef(fm)
  
  if(ncol(Y) > 1)
    Yortho <- fm$residuals
  else
    Yortho <- matrix(fm$residuals, ncol = 1, 
      dimnames = list(row.names(Y), colnames(Y)))
  
  list(Y = Yortho, b = b)
  
  }

