headm <- function(X) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  nmax <- 6
  pmax <- 5
  
  print(X[1:min(n, nmax), 1:min(p, pmax)])
  if(n > nmax) cat("...\n\n")
  
  cat("\nnrow =", n, "   ncol =", p, "\n\n")
  
  }
  
  