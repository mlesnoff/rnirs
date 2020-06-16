kpca <- function(Xr, Xu = NULL, ncomp, kernel = NULL, ...) {
  
  X <- .matrix(Xr)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  .k <- kernel
  if(is.null(.k))
    .k <- .krbf
  
  K <- .k(X, ...)
  Kc <- t(t(K - colSums(K) / n) -  rowSums(K) / n) + sum(K) / n^2
  #J <- matrix(1, nrow = n, ncol = n) / n
  #Kc <- K - J %*% K - K %*% J + J %*% K %*% J
  
  res <- eigen(Kc / n)
  
  eig <- res$values[1:ncomp]
  sv <- sqrt(eig)
  xsstot <- sum(res$values)

  A <- .scale(res$vectors[, 1:ncomp, drop = FALSE], scale = sv)
  
  T <- Kc %*% A
  
  z <- data.frame(ncomp = 1:ncomp, var = eig, pvar = eig / xsstot)
  z$cumpvar <- cumsum(z$pvar)
  row.names(z) <- 1:ncomp
  explvarx <- z
  
  Tu <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)
    m <- dim(Xu)[1]
    
    Ku <- .k(Xu, X, ...)
    
    Kuc <- t(t(Ku - rowSums(Ku) / n) - rowSums(K) / n) + sum(K) / (n * n) 
    #Ju <- matrix(1, nrow = m, ncol = n) / n
    #Kuc <- Ku - Ju %*% K - Ku %*% J + Ju %*% K %*% J
    
    Tu <- Kuc %*% A
    
    }
  
  list(Tr = T, Tu = Tu, A = A, 
    eig = eig, sv = sv, explvarx = explvarx, values = res$values,
    weights = rep(1 / n, n), T.ortho = TRUE) 
  
  }

