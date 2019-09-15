dis <- function(Xr, mu = NULL, Xu = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  sigma = NULL, out = c("mad", "sd", "boxplot"), cri = 3) {
  
  diss <- match.arg(diss)
  out <- match.arg(out)
  
  X <- .matrix(Xr, prefix.colnam = "x")                                              
  n <- nrow(X)
  p <- ncol(X)
  rownam <- row.names(X)
  
  if(is.null(mu)) mu <- colMeans(X)
  
  if(diss == "euclidean") d <- sqrt(.dis(X, mu))

  if(diss == "mahalanobis") {
      if(is.null(sigma)) sigma <- cov(X)
      U <- chol(sigma)
      d <- sqrt(.mah(X, mu, U))
      }
  
  if(diss == "correlation") {
      if(p > 1) {
        rho <- cor(t(X), mu)
        d <- sqrt(.5 * (1 - rho))
        } 
      else d <- sqrt(.dis(X, mu))
      }

  cut <- switch(
    out, 
    mad = median(d) + cri * mad(d), 
    sd = mean(d) + cri * sd(d),
    boxplot = {u <- fivenum(d) ; u <- u[4] + 1.5 * diff(u[c(2, 4)])}
    )
  
  z <- data.frame(rownum = 1:n, rownam = rownam, ncomp = rep(p, n), d = c(d))
  z$dstand <- z$d / cut
  dr <- z
  
  ### NEW OBSERVATIONS
  
  du <- NULL
  if(!is.null(Xu)) {
    
    X <- .matrix(Xu, prefix.colnam = "x")                                              
    m <- nrow(X)
    rownam <- row.names(X)
    
    d <- switch(diss,
      
      euclidean = sqrt(.dis(X, mu)),

      mahalanobis = sqrt(.mah(X, mu, U)),
      
      correlation = {
        if(p > 1) {
          rho <- cor(t(X), mu)
          sqrt(.5 * (1 - rho))
          } 
        else sqrt(.dis(X, mu))
        }
      
      )
    
    z <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(p, m), d = c(d))
    z$dstand <- z$d / cut
    du <- z
    
    }

  ### END

  list(dr = dr, du = du)
  
  }




