plsrcp <- function(X, Y, ncomp, algo = pls.kernel,
                   typ = c("aic", "aicc", "bic", "fpe", "gcv"),
                   methdf = c("cov", "sens", "naive"),
                   B = 50, eps = 1e-4, seed = NULL,
                   theta = 3, 
                   print = TRUE, ...) {
  
  typ <- match.arg(typ) 
  methdf <- match.arg(methdf) 
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo, ...)
  z <- mse(fm, ~ ncomp)
  ssr <- z$nbpred * z$msep
  
  if(methdf == "cov") 
    df <- plsr.dfcov(X, Y, ncomp = ncomp, algo = algo, 
                    B = B, seed = seed, print = print, ...)$df
  if(methdf == "sens") 
    df <- plsr.dfsens(X, Y, ncomp = ncomp, algo = algo, 
                     ns = B, eps = eps, seed = seed, print = print, ...)$df
  if(methdf == "naive")
    df <- theta * (0:ncomp) + 1
  df.ssr <- n - df
  
  s2.0 <- ssr[ncomp + 1] / df.ssr[ncomp + 1]
  
  r <- switch(
    typ,
    aic = ssr + 2 * s2.0 * df,
    aicc = ssr + 2 * s2.0 * df * n / (n - df - 1),
    bic = ssr + log(n) * s2.0 * df,
    fpe = (n + df) / (n - df) * ssr,
    gcv = n / (df.ssr)^2 * ssr
    )
  
  #r <- n * log(ssr) + 2 * df
  #r <- n * log(ssr) + 2 * df * n / (n - df - 1)  
  #r <- n * log(ssr) + log(n) * df
  
  if(typ != "gcv")
    r <- r / n
  
  delta <- r - min(r)
  z <- exp(-.5 * delta)
  w <- z / sum(z)

  res <- data.frame(
    ncomp = 0:ncomp, n = rep(n, ncomp + 1),
    ssr = ssr, df = df, df.ssr = df.ssr,
    err = r, delta = delta, w = w
    )
  
  }
