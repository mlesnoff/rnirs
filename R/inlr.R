inlr <- function(Xr, Xu = NULL, degree = 2, scale.blocks = FALSE) {
  
  if(degree < 2)
    stop("Argument 'degree' must be an integer > 1.")
  
  Xr <- .matrix(Xr)
  p <- dim(Xr)[2]
  zcol <- sort(rep(1:degree, p))
  
  .cbindpow <- function(X, degree) {
    Y <- zX <- X
    for(i in 1:(degree - 1)){
      zX <- X * zX
      Y <- cbind(Y, zX)
      }
    Y
    }

  Xr <- .cbindpow(Xr, degree = degree)
  colnames(Xr) <- paste(colnames(Xr), zcol, sep = "." )
  
  if(!is.null(Xu)) {
    Xu <- .cbindpow(.matrix(Xu), degree = degree)
    colnames(Xu) <- colnames(Xr)
    }
  
  if(scale.blocks) {
    res <- blockscal(Xr, Xu, colblocks = zcol)
    Xr <- res$Xr
    Xu <- res$Xu
    }
  
  list(Xr = Xr, Xu = Xu, colblocks = zcol)
  
  }
