bcoef <- function(fm) {
  
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  b <- fm$R %*% t(fm$C)
  
  int <- fm$ymeans - t(fm$xmeans) %*% b
  
  b <- rbind(int, b)
  row.names(b)[1] <- "intercept"
  colnames(b) <- row.names(fm$C)
  
  b

  }