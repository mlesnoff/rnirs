lwplsr <- function(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  ncomp,
  print = TRUE,
  ...
  ) {

  diss <- match.arg(diss)
  
  Xr <- .matrix(Xr, prefix.colnam = "x")
  Xu <- .matrix(Xu, prefix.colnam = "x")
  n <- nrow(Xr)
  m <- nrow(Xu)

  if(is.null(ncompdis)) ncompdis <- 0
  ncompdis <- sort(unique(ncompdis))
  h <- sort(unique(h))
  k <- sort(unique(ifelse(k > n, n, k)))
  
  param <- expand.grid(ncompdis, h, k)
  names(param) <- c("ncompdis", "h", "k")
  npar <- nrow(param)
  
  fm <- r <- fit <- y <- vector(mode = "list", length = npar)
  for(i in 1:npar) {
    
    if(print) {
      cat("\nparam ", "(", i, "/", npar, ") \n", sep = "")
      print(param[i, ]) ; cat("\n")
      }
    
    zncompdis <- param$ncompdis[i]
    zh <- param$h[i]
    zk <- param$k[i]
    
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
      fun = plsr,
      algo = pls.kernelw,
      ncomp = ncomp,
      print = print,
      ...
      )
    
    nr <- nrow(zfm$y)
    z <- data.frame(
      ncompdis = rep(zncompdis, nr),
      h = rep(zh, nr)
      )

    y[[i]] <- cbind(z, zfm$y)
    fit[[i]] <- cbind(z, zfm$fit)
    r[[i]] <- cbind(z, zfm$r)
    
    fm[[i]] <- zfm$fm
    
    gc()
  
    }
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  if(print) cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm)
  
  }
  
  