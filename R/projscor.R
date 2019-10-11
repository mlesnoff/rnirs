projscor <- function(X, fm) {
  
  T <- scale(.matrix(X), center = fm$xmeans, scale = FALSE) %*% fm$R
    
  rownam <- row.names(X)
  colnam <- paste("comp", 1:ncol(T), sep = "")
  
  dimnames(T) <- list(rownam, colnam)
  
  T
  
  }
    
