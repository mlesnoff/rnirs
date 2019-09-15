dadis <- function(Xr, Yr, Xu, Yu = NULL,
  diss = c("euclidean", "mahalanobis", "correlation"), sigma = NULL){
  
  diss <- match.arg(diss)

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
    centers <- d <- NULL
    }
  ### END
  
  else {
  
    centers <- centr(Xr, Yr)$centers
    if(diss == "mahalanobis" & is.null(sigma)) Wi <- matW(Xr, Yr)$Wi
  
    for(i in 1:nclas) {
    
      if(diss %in% c("euclidean", "correlation"))
        zd <- dis(Xu, centers[i, ], diss = diss)$dr$d
  
      if(diss == "mahalanobis") {
        
        ### CASE WHERE ni[i]=1
        if(is.null(sigma)) {
           
          if(ni[i] > 1) zsigma <- Wi[[i]] * (ni[i] - 1) / ni[i]
          else zsigma <- Wi[[i]]
          
          }
        else zsigma <- as.matrix(sigma)
        ### END
        
        ### IF SIGMA IS SINGULAR ==> THE OPTION IS TO REPLACE SIGMA BY DIAG(SIGMA)
        U <- tryCatch(chol(zsigma), error = function(e) e)
        if(inherits(U, "error")) {
          zsigma <- diag(diag(zsigma), nrow = p)
          U <- sqrt(zsigma)
          }
        ### END
        
        zd <- .mah(Xu, centers[i, ], U)
        
        }
    
      if(i == 1) d <- zd else d <- cbind(d, zd)
    
      }

  if(is.vector(d)) d <- matrix(d, nrow = 1)
  
  colnames(d) <- namclas
  
  # if ex-aequos, the first is selected
  z <- apply(d, FUN = function(x) which.min(x), MARGIN = 1) 
  fit <- sapply(z, FUN = function(x) namclas[x])
  
  }
  
  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = 1:m, rownam = rownam.Xu, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = 1:m, rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = 1:m, rownam = rownam.Xu, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr
  
  list(y = y, fit = fit, r = r, d = d, centers = centers, ni = ni)
  
  }