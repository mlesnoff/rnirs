bcoef <- function(fm, Y = NULL, ncomp = NULL) {
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  if(is.null(ncomp))
    ncomp <- dim(fm$Tr)[2]
  
  if(!is.null(fm$C) & fm$T.ortho) {
    beta <- t(fm$C)[1:ncomp, , drop = FALSE]
    colnam.Y <- row.names(fm$C)
    }
  else {
    Y <- .matrix(Y, row = FALSE, prefix.colnam = "y")
    fm$ymeans <- .xmean(Y, fm$weights)
    Y <- .center(Y, fm$ymeans)
    z <- coef(lm(Y ~ fm$Tr[, 1:ncomp, drop = FALSE] - 1, weights = fm$weights))
    beta <- matrix(z, nrow = ncomp, ncol = dim(Y)[2])
    colnam.Y <- colnames(Y)
    }
  
  row.names(beta) <- paste("comp", 1:ncomp, sep = "")
  colnames(beta) <- colnam.Y
  
  b <- fm$R[, 1:ncomp, drop = FALSE] %*% beta
  int <- fm$ymeans - t(fm$xmeans) %*% b
  b <- rbind(int, b)
  row.names(b)[1] <- "intercept"
  
  list(beta = beta, b = b)
  
  }