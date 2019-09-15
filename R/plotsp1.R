plotsp1 <- function(X, coord.fixed = FALSE, col = "blue", main = "Row", ...) {

  X <- .matrix(X)
  n <- nrow(X)
  
  a <- ""
  i <- 1
  while(a == "") {
    
    a <- readLines(n = 1)
    
    if(!is.null(main)) zmain <- paste(main, i)
    else zmain <- NULL
    
    if(!coord.fixed)
      plotsp(X[i, , drop = FALSE], col = col, main = zmain, ...)
    else
      plotsp(X, rownum = i, col = col, main = zmain, ...)
      
    
    i <- i + 1
    if(i > nrow(X)) a <- "stop"
  
    }
  
  }



