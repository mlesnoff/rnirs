odis <- function(fm, Xr, Xu = NULL) {
    
  if(!is.null(fm$fm))
    fm <- fm$fm
  
  if(is.null(fm$Tr))
    fm$Tr <- fm$T
  
  out <- match.arg(out)
  
  X <- .matrix(Xr)
  n <- dim(X)[1]
  rownam <- row.names(X)
  
  X <- scale(X, center = fm$xmeans, scale = FALSE)
  
  ncomp <- dim(fm$Tr)[2]

  E <- X - tcrossprod(fm$Tr, fm$P)
  
  d <- sqrt(rowSums(E * E))

  zmed <- median(d)
  zmad <- mad(d)
  dstand <- abs(d - zmed) / zmad 
  
  dr <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(ncomp, n), 
    d = d, dstand = dstand)
  rownames(dr) <- 1:n
  
  ### NEW OBSERVATIONS
  
  Eu <- du <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    rownam <- row.names(Xu)
    
    Tu <- .projscor(fm, Xu)
    Xu <- scale(Xu, center = fm$xmeans, scale = FALSE)
    
    Eu <- Xu - tcrossprod(Tu, fm$P)
    
    d <- sqrt(rowSums(Eu * Eu))
    
    dstand <- abs(d - zmed) / zmad 
    
    du <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(ncomp, m), 
      d = d, dstand = dstand)
    rownames(du) <- 1:m
    
    }
  
  ### END

  list(dr = dr, du = du)

  }