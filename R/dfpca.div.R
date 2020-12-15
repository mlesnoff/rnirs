dfpca.div <- function(X, ncomp, algo = NULL, 
                      ns = 50, eps = 1e-4,
                      seed = NULL, print = TRUE, ...) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  N <- n * p
  
  ## The sampling is done by crossing rows and columns
  ## for decreasing variability
  ns <- min(ns, n, p)
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp

 if(is.null(algo))
    if(n < p)
      algo <- pca.eigenk
    else
      algo <- pca.eigen  
  
  fm <- pca(X, ncomp = ncomp, algo = algo, ...)
  
  set.seed(seed = seed)
  srow <- sample(1:n, size = ns, replace = FALSE)
  scol <- sample(1:p, size = ns, replace = FALSE)
  set.seed(seed = NULL)
  
  SENS <- matrix(nrow = ns, ncol = ncomp)
  for(i in 1:ns) {
    
    if(print)
      cat(i, " ")
    
    zX <- X
    zX[srow[i], scol[i]] <- zX[srow[i], scol[i]] + eps

    zfm <- algo(zX, ncomp = ncomp)
    
    v <- numeric()
    for(a in 1:ncomp) {
    
      fit <- xfit(fm$Tr[, 1:a, drop = FALSE], 
              fm$P[, 1:a, drop = FALSE], fm$xmeans)
      zfit <- xfit(zfm$T[, 1:a, drop = FALSE], 
              zfm$P[, 1:a, drop = FALSE], zfm$xmeans)
      
      v[a] <- zfit[srow[i], scol[i]] - fit[srow[i], scol[i]]

      }
  
    SENS[i, ] <- v / eps
    
    }

  if(print)
    cat("\n\n")
  
  df <- colSums(SENS) * N / ns
  df <- c(p, df)
  
  
  dftrue <- p + zncomp * (n - 1) + p * zncomp - zncomp^2
  
  list(df = df, dftrue = dftrue)
  
  
  }


