cvpca.tri <- function(X, ncomp, algo = NULL, 
                      segm, 
                      print = TRUE, ...) {

  ### It also returns equal results to "Iterative ekf-TRI" with niter = 1
  ### = "Algorithm1" in Camacho & Ferrer 2014
  ### If iteration until convergence, "Iterative ekf-TRI" is numerically 
  ### equivalent to ekf-PMP (used in EigenVector PLS-Toolbox) in most cases
  ### PMP can bring numerical unstabillity in ekf (e.g. Fig.2-c)
  ### Annex functions "pca.cvtri.iter" and "pca.cvtri.pmp"


  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  N <- n * p
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp
  
  if(is.null(algo))
    if(n < p)
      algo <- pca.eigenk
    else
      algo <- pca.eigen
  
  nrep <- length(segm)
  nsegm <- length(segm[[1]])

  res <- list()
  SSR <- matrix(nrow = nsegm, ncol = ncomp + 1)
  for(i in 1:nrep) {
    
    if(print)
      cat("/ rep=", i, " ", sep = "") 
    
    zsegm <- segm[[i]]
    
    for(j in 1:nsegm) {
      
      s <- sort(zsegm[[j]])
      ns <- length(s)
      
      if(print)
        cat("segm=", j, " ", sep = "")
      
      fm <- algo(X[-s, , drop = FALSE], ncomp = ncomp, ...)
      
      zX <- .center(X[s, , drop = FALSE], fm$xmeans)
      ssr0 <- sum(zX * zX) / (ns * p)
      
      E <- matrix(nrow = ns, ncol = p)
      for(a in 1:ncomp) {
        
        zT <- zX %*% fm$P[, 1:a, drop = FALSE]
        zXfit <- tcrossprod(zT, fm$P[, 1:a, drop = FALSE])
        R <- zX - zXfit
        
        for(k in 1:p) {
          
          Qk <- crossprod(fm$P[k, 1:a])
          
          E[, k] <- zX[, k] * c(Qk) + R[, k]
        
          }
            
        SSR[j, a + 1] <- sum(E * E)
        
        }
    
      SSR[j, ] <- SSR[j, ] / (ns * p)
      SSR[j, 1] <- ssr0
      
      }
    
    res[[i]] <- data.frame(
      rep = rep(i, ncomp + 1),
      N = rep(N, ncomp + 1),
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
  
  z <- dtaggregate(ssr ~ ncomp + N, data = res, FUN = mean)
  z$msep <- dtaggregate(msep ~ ncomp + N, data = res, FUN = mean)$msep
  s2 <- dtaggregate(msep ~ ncomp + N, data = res, FUN = var)$msep
  z$se <- sqrt(s2 / nrep)
  res.summ <- z
  
  opt.summ <- z$ncomp[z$msep == min(z$msep)]
  
  list(res.summ = res.summ, opt.summ = opt.summ, 
       res = res, opt = opt)

  }
