daglm <- function(Xr, Yr, Xu, Yu = NULL, family = binomial(link = "logit")){
  
  Xr <- .matrix(Xr)
  n <- nrow(Xr)
  
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
  
  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m)
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(lev, m)
    dummyfit <- NULL
    }
  ### END
  
  else {
  
    Yrdummy <- dummy(Yr)
    
    z <- matrix(nrow = m, ncol = nclas)
    for(i in 1:nclas) {
      
      dat <- data.frame(Yr = Yrdummy[, i], Xr)
      fm <- suppressWarnings(glm(Yr ~ ., family = family, data = dat))
      #fm <- suppressWarnings(glm(Yr ~ ., family = family, data = dat, weights = weights))
      z[, i] <- predict(fm, newdata = data.frame(Xu), type = "response")
      
      }
    row.names(z) <- rownam.Xu
    colnames(z) <- lev
    dummyfit <- z
  
    z <- apply(dummyfit, FUN = .findmax, MARGIN = 1) 
    fit <- sapply(z, FUN = function(x) lev[x])
  
    }

  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = 1:m, rownam = rownam.Xu, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = 1:m, rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = 1:m, rownam = rownam.Xu, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Yr

  list(y = y, fit = fit, r = r, dummyfit = dummyfit, ni = ni)
  
  }