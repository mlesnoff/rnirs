dis <- function(Xr, Xu = NULL, mu = NULL, 
  diss = c("euclidean", "mahalanobis", "correlation"), sigma = NULL, 
  out = c("mad", "sd", "boxplot"), cri = 3) {
  
  diss <- match.arg(diss)
  out <- match.arg(out)
  
  Xr <- .matrix(Xr, prefix.colnam = "x")                                              
  n <- nrow(Xr)
  p <- ncol(Xr)
  rownam <- row.names(Xr)
  
  if(is.null(mu)) mu <- colMeans(Xr)

  if(diss == "euclidean") d <- sqrt(.dis(Xr, mu))

  if(diss == "mahalanobis") {
      if(is.null(sigma)) sigma <- cov(Xr)
      U <- chol(sigma)
      d <- sqrt(.mah(Xr, mu, U))
      }
  
  if(diss == "correlation") {
      if(p > 1) {
        rho <- cor(t(Xr), mu)
        d <- sqrt(.5 * (1 - rho))
        } 
      else d <- sqrt(.dis(Xr, mu))
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
    
    Xu <- .matrix(Xu, prefix.colnam = "x")                                              
    m <- nrow(Xu)
    rownam <- row.names(Xu)
    
    d <- switch(diss,
      
      euclidean = sqrt(.dis(Xu, mu)),

      mahalanobis = sqrt(.mah(Xu, mu, U)),
      
      correlation = {
        if(p > 1) {
          rho <- cor(t(Xu), mu)
          sqrt(.5 * (1 - rho))
          } 
        else sqrt(.dis(Xu, mu))
        }
      
      )
    
    du <- data.frame(rownum = 1:m, rownam = rownam, ncomp = rep(p, m), d = c(d))
    du$dstand <- du$d / cut
    
    }

  ### END

  list(dr = dr, du = du)
  
  }




