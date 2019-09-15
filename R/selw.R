selw <- function(X, ranges) {

  X <- .matrix(X)
  
  if(!is.list(ranges)) ranges <- list(ranges)

  rownam <- rownames(X)
  colnam <- colnames(X)
  
  # Xlist
  k <- length(ranges)
  Xlist <- vector("list", length = k)
  for(i in 1:k) {
    #i <- 2
    u <- ranges[[i]]
    u <- match(as.character(u[1]:u[2]), colnam)
    u <- u[!is.na(u)]
    u

    if(length(u) > 0)
      Xlist[[i]] <- X[, u, drop = FALSE]
    }
  names(Xlist) <- ranges
  
  # X
  for(i in 1:k)
    if(i == 1) X <- Xlist[[i]] else X <- cbind(X, Xlist[[i]])
  
  list(X = X, Xlist = Xlist)
  
  }

  
  
  



