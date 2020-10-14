pcacv <- function(X, ncomp, segm, algo = NULL, 
                      print = FALSE, ...) {
  
  X <- .matrix(X)
  zdim <- dim(X)
  n <- zdim[1]
  p <- zdim[2]
  
  ncomp <- min(ncomp, n, p)
  zncomp <- 0:ncomp
  
  if(is.null(algo))
    if(n < p)
      algo <- pca.eigenk
    else
      algo <- pca.eigen
  
  dof.mod <- n * zncomp 
  dof.ssr <- n * p - dof.mod
      
  nrep <- length(segm)
  res <- list()
  k <- 1
  for(i in 1:nrep) {
    
    zsegm <- segm[[i]]
    nsegm <- length(zsegm)
    
    for(j in 1:nsegm) {
      
      s <- sort(zsegm[[j]])
      ns <- length(s)
      
      if(print)
        cat("\n\n------------------------- Repetition: ", i, "  Segment: ", j,
          "\n\nRow numbers of X to predict: \ns =", s,
          "\n(The models are fitted on X[-s, ], Y[-s].)\n\n")
      
      zfm <- algo(X[-s, , drop = FALSE], ncomp = ncomp, ...)
      zT <- .projscor(zfm, X[s, , drop = FALSE])
      zX <- .center(X[s, , drop = FALSE], zfm$xmeans)
      
      SSR <- matrix(nrow = ns, ncol = ncomp + 1)
      SSR[, 1] <- rowSums(zX * zX)
      
      cat(k, " ")
      
      for(a in 1:ncomp) {
        E <- zX - tcrossprod(zT[, 1:a, drop = FALSE], 
                                    zfm$P[, 1:a, drop = FALSE])
        SSR[, a + 1] <- rowSums(E * E)
        }
      
      ssr <- colSums(SSR) * n / ns
      
      res[[k]] <- data.frame(
        rep = rep(i, ncomp + 1),
        segm = rep(j, ncomp + 1),
        ncomp = zncomp,
        ssr = ssr,
        dof.mod = dof.mod,
        dof.ssr = dof.ssr,
        msep = ssr / dof.ssr
        )
      
      k <- k + 1
      
      }
    
    }
  
  cat("\n\n")
  
  res <- setDF(rbindlist(res))

  s2 <- dtaggregate(msep ~ ncomp, data = res, FUN = var)$msep
  
  mse <- aggregate(cbind(ssr, dof.mod, dof.ssr, msep) ~ ncomp, 
                 data = res, FUN = mean)
  mse$se <- sqrt(s2 / (nrep * nsegm))
  
  list(mse = mse, mse.segm = res)

  }
