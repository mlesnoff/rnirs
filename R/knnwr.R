knnwr <- function(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  print = TRUE
  ) {

  diss <- match.arg(diss)
  
  Xr <- .matrix(Xr)
  Xu <- .matrix(Xu)
  n <- nrow(Xr)
  m <- nrow(Xu)

  if(is.null(ncompdis)) ncompdis <- 0
  ncompdis <- sort(unique(ncompdis))
  k <- sort(unique(ifelse(k > n, n, k)))
  h <- sort(unique(h))
  
  param <- expand.grid(ncompdis, h, k)
  names(param) <- c("ncompdis", "h", "k")
  npar <- nrow(param)
  
  r <- fit <- y <- vector(mode = "list", length = npar)
  for(i in 1:npar) {
    
    if(print) {
      cat("\nparam ", "(", i, "/", npar, ") \n", sep = "")
      print(param[i, ]) ; cat("\n")
      }
    
    zncompdis <- param$ncompdis[i]
    zk <- param$k[i]
    zh <- param$h[i]
  
    if(zncompdis == 0) {
      zresn <- getknn(Xr, Xu, k = zk, diss = diss)
      } else { 
        z <- pls.kernel(Xr, Yr, ncomp = zncompdis)
        zresn <- getknn(z$T, projscor(Xu, z), k = zk, diss = diss)
        }
    
    zfm <- locw(
      Xr, Yr,
      Xu, Yu,
      listnn = zresn$listnn,
      listw = lapply(zresn$listd, wkern, h = zh),
      fun = .knnr,
      print = print
      )
    
    nr <- nrow(zfm$y)
    z <- data.frame(
      ncompdis = rep(zncompdis, nr),
      h = rep(zh, nr)
      )

    y[[i]] <- cbind(z, zfm$y)
    fit[[i]] <- cbind(z, zfm$fit)
    r[[i]] <- cbind(z, zfm$r)

    rm(zfm)
    
    gc()
    
    }
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  if(print) cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r)

  }
  
  