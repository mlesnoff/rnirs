dis <- function( Xr, Xu = NULL, mu = NULL, 
  diss = c("euclidean", "mahalanobis", "correlation"), sigma = NULL, 
  weights = NULL) {
  
  diss <- match.arg(diss)
  
  X <- .matrix(Xr)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  rownam <- row.names(X)
  
  if(is.null(mu)) 
    mu <- .xmean(X, weights = weights)

  if(diss == "euclidean")
    d <- sqrt(.dis(X, mu))

  if(diss == "mahalanobis") {
      
    if(is.null(sigma)) {
      
      if(is.null(weights))
        sigma <- cov(X) * (n - 1) / n
      else 
        sigma <- .xcov(X, weights = weights)
      
      }
    
    U <- chol(sigma)
    d <- sqrt(.mah(X, mu, U))
      
    }
  
  if(diss == "correlation") {
    
    if(p > 1) {
      rho <- cor(t(X), mu)
      d <- sqrt(.5 * (1 - rho))
      } 
    else 
      d <- sqrt(.dis(X, mu))
    
    }

  d <- c(d)
  zmed <- median(d)
  zmad <- mad(d)
  dstand <- abs(d - zmed) / zmad 
  
  dr <- data.frame(rownum = 1:n, rownam = rownam, 
    ncomp = rep(p, n), d = d, dstand = dstand)
  
  ### NEW OBSERVATIONS
  
  du <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)                                              
    m <- dim(Xu)[1]
    rownam <- row.names(Xu)
    
    d <- switch(diss,
      
      euclidean = sqrt(.dis(Xu, mu)),

      mahalanobis = sqrt(.mah(Xu, mu, U)),
      
      correlation = {
        if(p > 1) {
          rho <- cor(t(Xu), mu)
          sqrt(.5 * (1 - rho))
          } 
        else 
          sqrt(.dis(Xu, mu))
        }
      
      )
    
    d <- c(d)
    dstand <- abs(d - zmed) / zmad 
      
    du <- data.frame(rownum = 1:m, rownam = rownam,
      ncomp = rep(p, m), d = d, dstand = dstand)
    
    }

  ### END

  list(dr = dr, du = du)
  
  }




