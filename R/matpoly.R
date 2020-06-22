matpoly <- function(X, degree = 2, blockscal = FALSE) {
  
  Y <- X <- .matrix(X)
  p <- dim(X)[2]

  if(degree > 1) {
    zX <- X
    for(i in 1:(degree - 1)){
      zX <- X * zX
      Y <- cbind(Y, zX)
      }
    }
  
  zcol <- sort(rep(1:degree, p))
  if(blockscal)
    Y <- blockscal(Y, colblocks = zcol)$Xr

  colnames(Y) <- paste(colnames(X), zcol, sep = "." )
  
  
  list(X = Y, colblocks = zcol)
  
  }
