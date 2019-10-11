dasdod <- function(Xr, Yr, Xu, Yu = NULL, ncomp.pls = NULL, ncomp.pca, nmin = 5,
  typcut = c("overall", "class"), pvar = NULL, cri = 3, ...){
  
  if(nmin < 2) stop("\nArgument nmin must be >= 2.\n\n")
  
  typcut <- match.arg(typcut)
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  p <- ncol(Xr)
  
  Xu <- .matrix(Xu)
  m <- nrow(Xu)
  rownam.Xu <- row.names(Xu)
  
  colnam.Yr <- colnames(Yr)
  if(is.null(colnam.Yr)) colnam.Yr <- "y1"
  
  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)
  
  namclas <- as.character(levels(Yr))
  
  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m)
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(namclas, m)
    pvarcla <- ncompcla <- d <- od.stand <- sd.stand <- NULL
    }
  ### END
  
  else {
    
    if(!is.null(ncomp.pls)) {
      fm <- pls.kernel(Xr, dummy(Yr), ncomp = ncomp.pls)
      Xr <- fm$T
      Xu <- projscor(Xu, fm)
      }
      
    pvarcla <- ncompcla <- vector(length = nclas)
    
    for(i in 1:nclas) {
      
      u <- which(Yr == namclas[i])
      zn <- length(u)
      
      if(zn < nmin) {
        
        zod <- zsd <- rep(NA, m)
      
        }
      
      else {
      
        zncomp <- min(ncomp.pca, zn - 1, p - 1)
        
        fm <- pca(Xr[u, ], ncomp = zncomp, ...)
        z <- fm$explvar
        
        if(!is.null(pvar)) {
          
          zpvar <- min(c(max(z$cumpvar), pvar))
          zncomp <- min(z$ncomp[z$cumpvar >= zpvar])
        
          }
        
        ncompcla[i] <- zncomp
        pvarcla[i] <- z$cumpvar[z$ncomp == zncomp]
        
        z <- sdod(Xr[u, ], Xu, fm, ncomp = zncomp, cri = cri)
        
        if(typcut == "overall") {
          zsd <- z$sdu$d
          zod <- z$odu$d
          zsd <- zsd / zncomp
          }
        
        if(typcut == "class") {
          zsd <- z$sdu$dstand
          zod <- z$odu$dstand
          }
      
        }
      
      if(i == 1) sd <- zsd else sd <- cbind(sd, zsd)
      if(i == 1) od <- zod else od <- cbind(od, zod)
    
      }
    
    if(typcut == "overall") {
      cut <- median(sd, na.rm = TRUE) + cri * mad(sd, na.rm = TRUE)
      sd.stand <- sd / cut
      cut <- median(od, na.rm = TRUE) + cri * mad(od, na.rm = TRUE)
      od.stand <- od / cut
      }
  
    if(typcut == "class") {
      sd.stand <- sd
      od.stand <- od
      }
      
    theta <- .5
    colnames(od.stand) <- colnames(sd.stand) <- namclas
    d <- sqrt(theta * sd.stand^2 + (1 - theta) * od.stand^2)
  
    # if ex-aequos, the first is selected
    z <- apply(d, FUN = function(x) which.min(x), MARGIN = 1) 
    fit <- sapply(z, FUN = function(x) namclas[x])
  
    }

  dat <- data.frame(
    rownum = 1:m,
    rownam = rownam.Xu
    )  
  
  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- cbind(dat, y, stringsAsFactors = FALSE)
  fit <- cbind(dat, fit, stringsAsFactors = FALSE)
  r <- cbind(dat, r)
  
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  list(y = y, fit = fit, r = r, d = d, sd.stand = sd.stand, od.stand = od.stand, ni = ni, 
    ncompcla = ncompcla, pvarcla = pvarcla)
    
  }