bcoef <- function(fm, ncomp = NULL) {
  
  if(is.null(ncomp)) ncomp <- ncol(fm$C)
  
  beta <- t(fm$C)
  
  zb <- fm$R[, 1:ncomp, drop = FALSE] %*% beta[1:ncomp,  ]
  
  int <- fm$ymeans - t(fm$xmeans) %*% zb
  
  b <- rbind(int, zb)
  row.names(b)[1] <- "intercept"
  colnames(b) <- row.names(fm$C)
  
  b

  }