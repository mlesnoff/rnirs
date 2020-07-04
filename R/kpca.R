kpca <- function(Xr, Xu = NULL, ncomp, kern = kpol, weights = NULL, ...) {
  
  Xr <- .matrix(Xr)
  n <- dim(Xr)[1]
  
  if(is.null(weights))
    weights <- rep(1 / n, n)
  else
    weights <- weights / sum(weights)
  
  K <- kern(Xr, ...)
  tK <- t(K)
  zK <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
    sum(weights * t(weights * tK))
  Kd <- sqrt(weights) * t(sqrt(weights) * t(zK))

  fm <- eigen(Kd)
  
  A <- fm$vectors[, 1:ncomp]
  eig <- fm$values[1:ncomp]
  sv <- sqrt(eig)
  xsstot <- sum(fm$values)
  
  Pr <- sqrt(weights) * .scale(A, scale = sv)

  T <- zK %*% Pr
  
  z <- data.frame(ncomp = 1:ncomp, var = eig, pvar = eig / xsstot)
  z$cumpvar <- cumsum(z$pvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    
    Ku <- kern(Xu, Xr, ...)
    zKu <- t(t(Ku - colSums(weights * t(Ku))) - colSums(weights * tK)) + 
      sum(weights * t(weights * tK))

    Tu <- zKu %*% Pr
    
    row.names(Tu) <- row.names(Xu)
    colnames(Tu) <-  paste("comp", 1:ncomp, sep = "")
    
    }
  
  row.names(A) <- row.names(T) <- row.names(Xr)
  colnames(T) <- colnames(A) <-  paste("comp", 1:ncomp, sep = "")
  
  list(Tr = T, Tu = Tu, 
    eig = eig, sv = sv, explvarx = explvarx, values = fm$values,
    weights = weights, T.ortho = TRUE) 
  
  }

