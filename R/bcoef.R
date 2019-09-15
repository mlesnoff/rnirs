bcoef <- function(fm, ncomp = NULL) {

  R <- fm$R
  C <- fm$C
  xmeans <- fm$xmeans
  ymeans <- fm$ymeans
  
  if(is.null(ncomp)) ncomp <- ncol(C)
  
  beta <- t(C)
  
  zb <- R[, 1:ncomp, drop = FALSE] %*% beta[1:ncomp,  ]
  
  int <- ymeans - t(xmeans) %*% zb
  
  b <- rbind(int, zb)
  row.names(b)[1] <- "intercept"
  colnames(b) <- colnames(beta)
  
  b
  
}