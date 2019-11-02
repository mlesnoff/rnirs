orthog <- function(X, Y, weights = NULL) {
  
  # Y is orthogonalized to X

  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  
  Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
  
  if(is.null(weights))
    weights <- rep(1, n)
  d <- weights / sum(weights)
  
  fm <- lm(Y ~ X, weights = d)
  
  if(ncol(Y) > 1)
    Yortho <- fm$residuals
  else
    Yortho <- matrix(fm$residuals, ncol = 1, 
      dimnames = list(row.names(Y), colnames(Y)))
  
  Yortho
  
  }

