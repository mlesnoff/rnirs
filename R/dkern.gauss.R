dkern.gauss <- function(Xr, Xu, H = NULL, hs = NULL, a = .5) { 
  
  Xr <- .matrix(Xr, row = FALSE, prefix.colnam = "x")
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Xu <- .matrix(Xu, row = FALSE, prefix.colnam = "x")
  m <- nrow(Xu)
  rownam.Xu <- rownames(Xu)
  
  ###### CASE WHERE n=1 
  if(n == 1) H <- diag(a * n^(-1/(p + 4)), nrow = p, ncol = p)
  ###### END

  if(is.null(H)) {
    if(!is.null(hs)) H <- diag(rep(hs, p), nrow = p)
      else {
        z <- matrixStats::colSds(Xr)
        z <- a * n^(-1/(p + 4)) * z               # a = .9, 1.06
        H <- diag(z, nrow = p)
        #e <- eigen(cov(Xr))
        #V <- e$vectors
        #Z <- V %*% diag(sqrt(e$values)) %*% t(V)
        #H <- a * n^(-1/(p + 4)) * Z
        }
      }

  invH <- solve(H)
  detH <- det(H)
  if(detH == 0) detH <- 1e-20
  
  for(i in 1:m) {
    
    z <- Xu[i, ]
    z <- matrix(rep(z, n), nrow = n, byrow = TRUE)
    z <- z - Xr
    z <- z %*% invH
    z <- z * z
    z <- rowSums(z)
    
    d <- 1 / n * (2 * pi)^(-p / 2) * (1 / detH) * sum(exp(-.5 * z))
    #d <- 1 / n * (2 * pi)^(-1) * (1 / detH) * sum(exp(-.5 * z))

    if(i == 1) fit <- d else fit <- c(fit, d)
    
    }
    
  fit <- data.frame(rownum = 1:m, rownam = rownam.Xu, fit = fit)
  
  list(fit = fit, H = H)
  
  }
  
  