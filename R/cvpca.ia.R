cvpca.ia <- function(X, ncomp, algo = NULL,
                     segm,
                     tol = .Machine$double.eps^0.5, 
                     maxit = 1, 
                     print = TRUE, ...) {

  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  N <- n * p
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp
  
  nrep <- length(segm)
  nsegm <- length(segm[[1]])
      
  X0 <- .center(X, matrixStats::colMeans2(X)) ## Add "weight"
  ssr0 <- sum(X0 * X0) / N
  
  res <- list()
  SSR <- matrix(nrow = nsegm, ncol = ncomp + 1)
  niter <- numeric()
  for(i in 1:nrep) {
    
    if(print)
      cat("/ rep=", i, " ", sep = "") 
    
    zsegm <- segm[[i]]
    
    for(j in 1:nsegm) {
      
      s <- sort(zsegm[[j]])
      ns <- length(s)
      
      if(print)
        cat("segm=", j, " ", sep = "")
      
      Xna <- X
      Xna[s] <- NA
      
      fm <- mdi.ia(Xna, ncomp = ncomp,
                   start = "nipals",
                   tol = tol, maxit = maxit, print = FALSE, ...)
      
      niter[j] <- fm$niter
      
      for(a in 1:ncomp) {
        
        Fit <- xfit(fm$T[, 1:a, drop = FALSE], fm$P[, 1:a, drop = FALSE], fm$xmeans)[s]
        
        E <- X[s] - Fit
        
        SSR[j, a + 1] <- sum(E * E)
        
        }
      
      SSR[j, ] <- SSR[j, ] / (ns * p)
      SSR[j, 1] <- ssr0
      
      }
    
    res[[i]] <- data.frame(
      rep = rep(i, ncomp + 1),
      ncomp = zncomp,
      ssr = (colSums(SSR) / nsegm) * N
      )
    
    }
  
  if(print)
    cat("/ End.")
  cat("\n\n")
  
  z <- setDF(rbindlist(res))
  z$msep <- z$ssr / N
  res <- z
  
  opt <- numeric()
  for(i in 1:nrep) {
    u <- z[z$rep == i, ]
    opt[i] <- u$ncomp[u$msep == min(u$msep)]
    }
  
  ##### Summary
  
  z <- dtaggregate(ssr ~ ncomp, data = res, FUN = mean)
  z$msep <- dtaggregate(msep ~ ncomp, data = res, FUN = mean)$msep
  s2 <- dtaggregate(msep ~ ncomp, data = res, FUN = var)$msep
  z$se <- sqrt(s2 / nrep)
  res.summ <- z
  
  list(res.summ = res.summ, res = res, opt = opt, 
       niter = niter)

  }
