bcoef <- function(fm, ncomp = NULL) {
  
  if(!fm$T.ortho)
    stop("This function is not implemented for algorithms providing
      non orthogonal scores.") 
  
  if(is.null(ncomp))
    ncomp <- dim(fm$R)[2]
  
  if(!is.null(fm$C))
    beta <- t(fm$C)[1:ncomp, , drop = FALSE]
  else
    beta <- fm$beta[1:ncomp, , drop = FALSE]
    
  b <- fm$R[, 1:ncomp, drop = FALSE] %*% beta
  int <- fm$ymeans - t(fm$xmeans) %*% b
  b <- rbind(int, b)
  row.names(b)[1] <- "intercept"
  
  b
  
  }