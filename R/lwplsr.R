lwplsr <- function(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  ncomp,
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
  h <- sort(unique(h))
  k <- sort(unique(ifelse(k > n, n, k)))
  
  z <- expand.grid(ncompdis, h, k)
  names(z) <- c("ncompdis", "h", "k")
  param <- z
  npar <- nrow(param)
  
  resn <- r <- fit <- y <- vector(mode = "list", length = npar)
  for(i in 1:nrow(param)) {
    
    if(print) {
      cat("\nparam ", "(", i, "/", npar, ") \n", sep = "")
      print(param[i, ]) ; cat("\n")
      }
    
    zncompdis <- param$ncompdis[i]
    zh <- param$h[i]
    zk <- param$k[i]
  
    if(zncompdis == 0) {
      Zr <- Xr
      Zu <- Xu
      } else { 
        z <- pls.kernel(Xr, Yr, ncomp = zncompdis)
        Zr <- z$T
        Zu <- projscor(Xu, z)
        }
    
    zresn <- getknn(Zr, Zu, k = zk, diss = diss)
    zlistnn <- zresn$listnn
    zlistw <- lapply(zresn$listd, wkern, h = zh)
    
    zfm <- locw(
      Xr, Yr,
      Xu, Yu,
      listnn = zlistnn,
      listw = zlistw,
      fun = plsr,
      algo = pls.kernelw,
      ncomp = ncomp,
      stor = stor,
      print = print
      )
    
    zy <- zfm$y
    zfit <- zfm$fit
    zr <- zfm$r
    
    u <- nrow(zy)
    u <- data.frame(
      ncompdis = rep(zncompdis, u),
      h = rep(zh, u)
      )
    zy <- cbind(u, zy)
    zfit <- cbind(u, zfit)
    zr <- cbind(u, zr)
    
    y[[i]] <- zy
    fit[[i]] <- zfit
    r[[i]] <- zr
    
    resn[[i]] <- zresn
    
    if(i == 1) fm <- zfm$fm else fm <- c(fm, zfm$fm)
    
    }
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  if(print) cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r, fm = fm, resn = resn)
  
  }
  
  