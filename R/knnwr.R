knnwr <- function(
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
  
  z <- expand.grid(ncompdis, h, k)
  names(z) <- c("ncompdis", "h", "k")
  zpar <- z
  npar <- nrow(zpar)
  
  res <- lapply(
    
    1:npar, function(i) {
    
      if(print) {
        cat("\nparam ", "(", i, "/", npar, ") \n", sep = "")
        print(zpar[i, ]) ; cat("\n")
        }
      
      zncompdis <- zpar$ncompdis[i]
      zk <- zpar$k[i]
      zh <- zpar$h[i]
    
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
        fun = .knnr,
        stor = stor,
        print = print
        )
    
      zy <- setDT(zfm$y)
      zfit <- setDT(zfm$fit)
      zr <- setDT(zfm$r)
      
      u <- nrow(zy)
      u <- data.table(
        ncompdis = rep(zncompdis, u),
        h = rep(zh, u)
        )
      y <- data.table(u, zy)
      fit <- data.table(u, zfit)
      r <- data.table(u, zr)
      
      fm <- zfm$fm
      
      return(list(y = y, fit = fit, r = r, resn = zresn, fm = fm))
  
      }
  
    )
  
  .f <- function(nam) {
    z <- lapply(1:length(res), function(i) {res[[i]][nam]})
    z <- unlist(z, recursive = FALSE)
    z <- setDF(rbindlist(z))
    }
  y <- .f("y")
  fit <- .f("fit")
  r <- .f("r")
  
  z <- lapply(1:length(res), function(i) {res[[i]]["resn"]})
  resn <- unlist(z, recursive = FALSE)
  names(resn) <- 1:npar

  z <- lapply(1:length(res), function(i) {res[[i]]["fm"]})
  fm <- unlist(z, recursive = FALSE)[[1]]
  
  if(print) cat("\n\n")
  
  gc()
  
  list(y = y, fit = fit, r = r, resn = resn, fm = fm)
  
  }
  
  