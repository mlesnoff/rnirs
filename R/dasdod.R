dasdod <- function(Xr, Yr, Xu, Yu = NULL, 
  ncomp, nmin = 5, theta = 0.5, 
  ...){
  
  if(nmin < 2) stop("\nArgument nmin must be >= 2.\n\n")
  
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
  
  # levels returns the sorted character level names 
  lev <- levels(Yr)      
  
  if(!is.null(Yu)) 
    Yu <- as.character(Yu) else Yu <- rep(NA, m)
  
  if(length(ncomp) == 1) 
    ncomp <- rep(ncomp, nclas)

  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(lev, m)
    pvarcla <- ncomp <- tabd <- odstand <- sdstand <- od <- sd <- NULL
    }
  ### END
  
  else {
      
    pvarcla <- cutod <- cutsd <- vector(length = nclas)
    od <- sd <- matrix(nrow = m, ncol = nclas)
    
    for(i in 1:nclas) {
      
      s <- which(Yr == lev[i])
      ns <- length(s)
      
      if(ns < nmin) {
        
        od[, i] <- sd[, i] <- rep(NA, m)
        cutod[i] <- cutsd[i] <- NA
      
        }
      
      else {
      
        ncomp[i] <- min(ncomp[i], ns - 1, p - 1)
        
        fm <- pca(Xr[s, ], Xu, ncomp = ncomp[i], ...)
        
        z <- fm$explvar
        pvarcla[i] <- z$cumpvar[dim(z)[1]]
        
        z <- scordis(fm)
        sd[, i] <- z$du$d
        cutsd[i] <- z$cutoff
        
        z <- odis(fm, Xr[s, ], Xu)
        od[, i] <- z$du$d
        cutod[i] <- z$cutoff
      
        }

      }
    
    sdstand <- scale(sd, center = FALSE, scale = cutsd)
    odstand <- scale(od, center = FALSE, scale = cutod)
    
    tabd <- sqrt(theta * sdstand^2 + (1 - theta) * odstand^2)
    
    z <- apply(-tabd, FUN = .findmax, MARGIN = 1) 
    fit <- sapply(z, FUN = function(x) lev[x])
  
    rownames(tabd) <- rownames(odstand) <- rownames(sdstand) <- rownames(od) <- rownames(sd) <- rownam.Xu
    colnames(tabd) <- colnames(odstand) <- colnames(sdstand) <- colnames(od) <- colnames(sd) <- lev
    
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
  
  list(y = y, fit = fit, r = r, tabd = tabd, sd = sd, od = od,
    sdstand = sdstand, odstand = odstand, cutsd = cutsd, cutod = cutod, 
    ncomp = ncomp, pvarcla = pvarcla, ni = ni)
    
  }