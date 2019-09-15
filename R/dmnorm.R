dmnorm <- function(Xr = NULL, Xu, mu = NULL, sigma = NULL, diag = FALSE) {
  
  Xr <- .matrix(Xr, row = FALSE, prefix.colnam = "x")
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Xu <- .matrix(Xu, row = FALSE, prefix.colnam = "x")
  m <- nrow(Xu)
  rownam.Xu <- rownames(Xu)

  if(is.null(mu)) mu <- colMeans(Xr)
  
  ###### CASE WHERE n=1 
  if(n == 1) sigma <- diag(1, nrow = p, ncol = p)
  ###### END
  
  if(is.null(sigma)) 
    sigma <- switch(as.character(diag),
      "FALSE" = cov(Xr),
      "TRUE"  = diag(apply(Xr, MARGIN = 2, var), nrow = p)
      ) 
  else
    sigma <- as.matrix(sigma)
  
  ####### IF SIGMA IS SINGULAR ==> THE OPTION IS TO REPLACE IT BY DIAG(SIGMA)
  U <- tryCatch(chol(sigma), error = function(e) e)
  if(inherits(U, "error")) 
    U <- sqrt(diag(diag(sigma), nrow = p))
  ####### END
  
  ####### IF U IS SINGULAR ==> THE OPTION IS TO REPLACE IT BY I(p)
  z <- tryCatch(solve(U), error = function(e) e)
  if(inherits(z, "error")) 
    U <- diag(p)
  ####### END

  d <- .mah(Xu, mu, U)
  
  zdet <- det(U)^2
  if(zdet == 0) zdet <- 1e-20

  ds <- (2 * pi)^(-p / 2) * (1 / sqrt(zdet)) * exp(-.5 * d)  # = density
  
  fit <- data.frame(rownum = 1:m, rownam = rownam.Xu, fit = c(ds))
  
  list(fit = fit)
  
  }

  
    
  