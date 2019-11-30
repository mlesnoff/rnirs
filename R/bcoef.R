bcoef <- function(fm) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  beta <- t(fm$C)
  
  zb <- fm$R %*% beta
  
  int <- fm$ymeans - t(fm$xmeans) %*% zb
  
  b <- rbind(int, zb)
  row.names(b)[1] <- "intercept"
  colnames(b) <- row.names(fm$C)
  
  b

  }