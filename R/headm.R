headm <- function(X) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  nmax <- 6
  pmax <- 6
  
  X <- X[1:min(n, nmax), 1:min(p, pmax), drop = FALSE]
  
  rownam <- row.names(X)
  colnam <- colnames(X)

  cla <- class(X)
  if(is.matrix(X)){
    
    if(is.null(rownam))
      rownam <- paste("[", 1:nrow(X), ",]", sep = "")
  
    if(is.null(colnam))
      colnam <- paste("[,", 1:ncol(X), "]", sep = "")
    
    }
  
  
  X <- data.frame(X)
  
  row.names(X) <- rownam
  names(X) <- colnam

  if(p > pmax)
    X <- cbind(X, OtherVariables = rep(".", nrow(X)))

  cat("\n\n")
  
  print(X)
  
  if(n > nmax) cat("...\n\n")
  
  cat("\nClass =", cla, "   nrow =", n, "   ncol =", p, "\n\n")
  
  }
  
  