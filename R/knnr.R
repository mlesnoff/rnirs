knnr <- function(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  stor = TRUE,
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
        zresn <- getknn(z$T, .projscor(z, Xu), k = zk, diss = diss)
        }
    
    zlistw <- lapply(zresn$listd, wdist, h = zh)
    
    zfm <- locw(
      Xr, Yr,
      Xu, Yu,
      listnn = zresn$listnn,
      listw = zlistw,
      fun = .knnr,
      stor = stor,
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

    if(i == 1) {
      fm <- zfm$fm
      listnn <- zresn$listnn
      listd <- zresn$listd
      listw <- zlistw
      }
    else {
      fm <- c(fm, zfm$fm)
      listnn <- c(listnn, zresn$listnn)
      listd <- c(listd, zresn$listd)
      listw <- c(listw, zlistw)
      }
      
    gc()
    
    }
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  resn <- list(listnn = listnn, listd = listd, listw = listw)
  
  if(print) cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm, resn = resn, param = param)

  }
  
  