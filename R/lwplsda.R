lwplsda <- function(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = 5, k,
  ncomp,
  cri = 5,
  stor = TRUE,
  print = TRUE,
  ...
  ) {

  diss <- match.arg(diss)
  
  Xr <- .matrix(Xr)
  Xu <- .matrix(Xu)
  n <- nrow(Xr)
  m <- nrow(Xu)

  nclas <- length(unique(Yr))
  
  if(is.null(ncompdis)) ncompdis <- 0
  ncompdis <- sort(unique(ncompdis))
  h <- sort(unique(h))
  k <- sort(unique(ifelse(k > n, n, k)))
  
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
    zh <- param$h[i]
    zk <- param$k[i]
  
    if(zncompdis == 0) {
      zresn <- getknn(Xr, Xu, k = zk, diss = diss)
      } else { 
          if(nclas == 1)
            z <- pca.eigen(Xr, ncomp = zncompdis)
          else
            z <- pls.kernel(Xr, dummy(Yr), ncomp = zncompdis)
        zresn <- getknn(z$T, .projscor(z, Xu), k = zk, diss = diss)
        }    
  
    zlistw <- lapply(zresn$listd, wdist, h = zh, cri = cri)
    
    zfm <- locw(
      Xr, Yr,
      Xu, Yu,
      listnn = zresn$listnn,
      listw = zlistw,
      fun = plsda,
      algo = pls.kernel,
      ncomp = ncomp,
      stor = stor,
      print = print,
      ...
      )
    
    nr <- dim(zfm$y)[1]
    z <- data.frame(
      ncompdis = rep(zncompdis, nr),
      h = rep(zh, nr)
      )

    y[[i]] <- cbind(z, zfm$y)
    fit[[i]] <- cbind(z, zfm$fit)
    r[[i]] <- cbind(z, zfm$r)

    if(i == 1) {
      fm <- zfm$fm ; listnn <- zresn$listnn ; 
      listd <- zresn$listd ; listw <- zlistw
      }
    else {
      fm <- c(fm, zfm$fm) ; listnn <- c(listnn, zresn$listnn) ; 
      listd <- c(listd, zresn$listd) ; listw <- c(listw, zlistw)
      }
      
    gc()
    
    }

  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  res.nn <- list(listnn = listnn, listd = listd, listw = listw)
  
  if(print)
    cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm, res.nn = res.nn, param = param)
  
  }    
    
