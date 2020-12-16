cpplsr <- function(X, Y, ncomp, algo = NULL,
                   type = c("aicc", "aic", "bic"),
                   methdf = c("cov", "div", "naive"),
                   B = 50, eps = 1e-4, seed = NULL,
                   theta = 3, 
                   print = TRUE, ...) {
  
  type <- match.arg(type) 
  methdf <- match.arg(methdf) 
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  fm <- plsr(X, Y, X, Y, ncomp = ncomp, algo = algo, ...)
  z <- mse(fm, ~ ncomp)
  ssr <- z$nbpred * z$msep
  
  if(methdf == "cov") 
    df <- dfplsrcov(X, Y, ncomp = ncomp, algo = algo, 
                    B = B, seed = seed, print = print, ...)$df
  
  if(methdf == "div") 
    df <- dfplsrdiv(X, Y, ncomp = ncomp, algo = algo, 
                     ns = B, eps = eps, seed = seed, print = print, ...)$df
  
  if(methdf == "naive")
    df <- 1 + theta * (0:ncomp)
  
  df.ssr <- n - df
  
  k <- min(ncomp, 30)
  s2 <- ssr[k + 1] / df.ssr[k + 1]
  
  r <- switch(
    type,
    aic = ssr + 2 * s2 * df,
    aicc = ssr + 2 * s2 * df * n / (n - df - 1),
    bic = ssr + log(n) * s2 * df
    ##fpe = (n + df) / (n - df) * ssr,
    ##gcv = n / (df.ssr)^2 * ssr
    )
  
  ##if(type != "gcv")
  ##  r <- r / n
  
  delta <- r - min(r)
  z <- exp(-.5 * delta)
  w <- z / sum(z)

  opt <- which(r == min(r))[1] - 1

  res <- data.frame(
    ncomp = 0:ncomp, n = rep(n, ncomp + 1),
    ssr = ssr, df = df, df.ssr = df.ssr,
    crit = r, delta = delta, w = w
    )
  
 list(res = res, opt = opt, k = k, s2 = s2)
  
  
  }
