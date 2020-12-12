plsrdf.cov <- function(X, Y, ncomp, algo = NULL,
                       B = 50, 
                       seed = NULL, print = TRUE, ...) {
  
  X <- .matrix(X)
  n <- dim(X)[1]

  fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo, ...)
  z <- mse(fm, ~ ncomp, digits = 25)
  ssr <- z$nbpred * z$msep
  ## Same as
  #fm <- algo(X, Y, ncomp = ncomp, ...)
  #ssr <- numeric()
  #for(a in 1:ncomp)
  #  ssr[a] <- sum(.resid.pls(fm, Y, ncomp = a)$r^2)
  ## End
  ## Low biased model estimate
  k <- min(ncomp, 30)
  s2 <- ssr[k + 1] / (k + 1)

  #s2 <- ssr[ncomp + 1] / (ncomp + 1)
    
  zY <- matrix(rep(Y, B), nrow = n, ncol = B, byrow = FALSE)
  set.seed(seed = seed)
  zE <- matrix(rnorm(n * B, sd = s2^.5), nrow = n, ncol = B)
  set.seed(seed = NULL)
  zY <- zY + zE
  
  Fit <- array(dim = c(n, B, ncomp)) 
  for(j in 1:B) {
    
    if(print)
      cat(j, " ")

    z <- plsr(X, zY[, j], X, zY[, j], ncomp = ncomp, algo = algo, ...)$fit
    zfit <- z[z$ncomp >= 1, ]
    
    Fit[, j, ] <- zfit[, ncol(zfit)]

    }
  
  if(print)
    cat("\n\n")
  
  Cov <- matrix(nrow = n, ncol = ncomp)
  for(a in 1:ncomp)
    for(i in 1:n)
      Cov[i, a] <- cov(zY[i, ], Fit[i, , a])
    
  cov <- colSums(Cov)
  df <- cov / s2
  
  df <- c(1, df)
  cov <- c(NA, cov)
  
  list(df = df, cov = cov, s2 = s2)
  
  }


