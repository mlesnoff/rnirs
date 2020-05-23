headm <- function(X) {
  
  if(is.null(X)) print(NULL)
  
  else {
    
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    nmax <- 6
    pmax <- 6
    
    cla <- class(X)
    
    if("AsIs" %in% cla) {
      X <- as.matrix(X)
      class(X) <- "matrix"
      }

    X <- X[1:min(n, nmax), 1:min(p, pmax), drop = FALSE]
    
    rownam <- row.names(X)
    colnam <- colnames(X)
    
    if(is.matrix(X)){
      
      if(is.null(rownam))
        rownam <- paste("[", 1:nrow(X), ",]", sep = "")
    
      if(is.null(colnam))
        colnam <- paste("[,", 1:ncol(X), "]", sep = "")
      
      }
    
    X <- as.data.frame(X)

    row.names(X) <- rownam
    colnames(X) <- colnam
  
    if(p > pmax)
      X <- cbind(X, OtherVariables = rep(".", nrow(X)))
  
    cat("\n\n")
    
    print(X)
    
    if(n > nmax) cat("...\n\n")
    
    cat("\nnrow =", n, " ncol =", p, "    class =", cla, "\n\n")
  
    }
  
  }
  
  