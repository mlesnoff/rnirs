blocksel <- function(X, blocks) {
  
    X <- .matrix(X)
    n <- dim(X)[1]
  
    nbl <- length(blocks)
  
    selcol <- unlist(blocks)
  
    colnam <- colnames(X)[selcol]
  
    X <- X[, selcol, drop = FALSE]
    colnames(X) <- colnam

    z <- lapply(seq_len(nbl), function(i) length(blocks[[i]]))
    lengthblock <- unlist(z)
  
    z <- lapply(seq_len(nbl), function(i) rep(i, lengthblock[i]))
    newcol <- data.frame(newcol = seq_len(sum(lengthblock)), block = unlist(z))
    newblocks <- lapply(seq_len(nbl), function(i) newcol$newcol[newcol$block == i])
  
    list(X = X, blocks = newblocks)  

    }