projscor <- function(X, fm) {
  
  X <- .matrix(X)
  
  R <- fm$R
  xmeans <- fm$xmeans
  
  X <- scale(X, center = xmeans, scale = FALSE)
  T <- X %*% R
    
  rownam <- row.names(X)
  colnam <- paste("comp", 1:ncol(T), sep = "")
  
  dimnames(T) <- list(rownam, colnam)
  
  T
  
  }
    
