blocksel <- function(X, blocks) {
  
  X <- .matrix(X, prefix.colnam = "x")
  n <- nrow(X)
  
  nbl <- length(blocks)
  selcol <- unlist(blocks)
  
  colnam <- colnames(X)[selcol]
  
  X <- X[, selcol, drop = FALSE]
  colnames(X) <- colnam

  z <- lapply(1:nbl, function(i) length(blocks[[i]]))
  lengthblock <- unlist(z)
  
  z <- lapply(1:nbl, function(i) rep(i, lengthblock[i]))
  newcol <- data.frame(newcol = 1:sum(lengthblock), block = unlist(z))
  newblocks <- lapply(1:nbl, function(i) newcol$newcol[newcol$block == i])
  
  list(X = X, blocks = newblocks)  

  }