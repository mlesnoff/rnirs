dfplsr.div <- function(X, Y, ncomp, algo = NULL, 
                       ns = 50, meth.samp = c("syst", "random"), eps = 1e-4, 
                       seed = NULL, print = TRUE, ...) {
  
  meth.samp <- match.arg(meth.samp)
  
  X <- .matrix(X)
  n <- dim(X)[1]
  
  ns <- min(ns, n) 
  
  y <- c(Y)
  eps <- mean(y) * eps
  
  fm <- plsr(X, y, X, ncomp = ncomp, algo = algo, ...)
  fit <- fm$fit
  
  if(meth.samp == "syst") {
    ## Regular sampling (grid) over y
    ## "order(y) = 3 7 etc." means: the 1st lower value is the component 3 of the vector y,
    ## the 2nd lower value is the component 7 of the vector y, etc.
    id <- order(y)
    u <- round(seq(1, n, length = ns))
    s <- sort(id[u])
    }
  
  if(meth.samp == "random") {
    set.seed(seed = seed)
    s <- sample(1:n, size = ns, replace = FALSE)
    set.seed(seed = NULL)
    }
  
  S <- matrix(nrow = ns, ncol = ncomp)
  for(i in 1:ns) {
    
    if(print)
      cat(i, " ")
    
    zs <- s[i]
    
    zy <- y
    zy[zs] <- y[zs] + eps
    
    zfm <- plsr(X, zy, X[zs, , drop = FALSE], ncomp = ncomp, algo = algo, ...)
    zfit <- zfm$fit
    
    v <- numeric()
    for(a in 1:ncomp) {
    
      fit.ref <- fit[fit$ncomp == a, ncol(fit)][zs]
      
      v[a] <- zfit[zfit$ncomp == a, ncol(zfit)] - fit.ref
      
      }
  
    S[i, ] <- v / eps
    
    }

  if(print)
    cat("\n\n")
  
  df <- colSums(S) * n / ns
  df <- c(1, df)
  
  list(df = df)
  
  }


