dis <- function(mu = NULL, Xr, Xu = NULL, 
  diss = c("euclidean", "mahalanobis", "correlation"), sigma = NULL, 
  out = c("mad", "sd", "boxplot"), cri = 3, weights = NULL) {
  
  diss <- match.arg(diss)
  out <- match.arg(out)
  
  X <- .matrix(Xr)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  rownam <- row.names(X)
  
  if(is.null(mu)) 
    mu <- .xmean(X, weights = weights)

  if(diss == "euclidean")
    d <- sqrt(.dis(mu, X))

  if(diss == "mahalanobis") {
      
    if(is.null(sigma)) {
      
      if(is.null(weights))
        sigma <- cov(X) * (n - 1) / n
      else 
        sigma <- .xcov(X, weights = weights)
      
      }
    
    U <- chol(sigma)
    d <- sqrt(.mah(mu, X, U))
      
    }
  
  if(diss == "correlation") {
    
    if(p > 1) {
      rho <- cor(t(X), mu)
      d <- sqrt(.5 * (1 - rho))
      } 
    else 
      d <- sqrt(.dis(mu, X))
    
    }

  cut <- switch(
    out, 
    mad = median(d) + cri * mad(d), 
    sd = mean(d) + cri * sd(d),
    boxplot = {z <- fivenum(d) ; z <- z[4] + 1.5 * diff(z[c(2, 4)])}
    )
  
  dr <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(p, n), d = c(d))
  dr$dstand <- dr$d / cut
  
  ### NEW OBSERVATIONS
  
  du <- NULL
  if(!is.null(Xu)) {
    
    Xu <- .matrix(Xu)                                              
    m <- dim(Xu)[1]
    rownam <- row.names(Xu)
    
    d <- switch(diss,
      
      euclidean = sqrt(.dis(mu, Xu)),

      mahalanobis = sqrt(.mah(mu, Xu, U)),
      
      correlation = {
        if(p > 1) {
          rho <- cor(t(Xu), mu)
          sqrt(.5 * (1 - rho))
          } 
        else 
          sqrt(.dis(mu, Xu))
        }
      
      )
    
    du <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(p, m), d = c(d))
    du$dstand <- du$d / cut
    
    }

  ### END

  list(dr = dr, du = du, cut = cut)
  
  }




