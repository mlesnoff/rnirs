daprob <- function(Xr, Yr, Xu, Yu = NULL, dens = dmnorm, 
  lda = TRUE, prior = c("uniform", "proportional"), ...){
  
  .dens <- match.fun(FUN = dens)
  
  dots <- list(...)
  namdot <- names(dots)

  Xr <- .matrix(Xr)
  zdim <- dim(Xr)
  n <- zdim[1]
  p <- zdim[2]
  
  Xu <- .matrix(Xu)
  m <- dim(Xu)[1]
  rownam.Xu <- row.names(Xu)
  
  colnam.Y <- colnames(Yr)
  if(is.null(colnam.Y)) 
    colnam.Y <- "y1"
  
  Yr <- as.factor(Yr)
  ni <- c(table(Yr))
  nclas <- length(ni)
  
  # levels returns the sorted character level names 
  lev <- levels(Yr)      
  
  if(!is.null(Yu)) Yu <- as.character(Yu) else Yu <- rep(NA, m) 
  
  ### CASE WHERE ALL THE TRAINING OBSERVATIONS HAVE THE SAME CLASS
  if(nclas == 1) {
    fit <- rep(lev, m)
    fm <- posterior <- prior <- ds <- NULL
    }
  ### END
  
  else {

    if(is.character(prior))
      prior <- switch(
        match.arg(prior), 
        uniform = rep(1 / nclas, nclas),
        proportional = ni / sum(ni) 
        )

    if(identical(.dens, dmnorm) & lda)  
        W <- matW(Xr, Yr)$W * n / (n - nclas)
    else
        W <- NULL

    ds <- matrix(nrow = m, ncol = nclas)
    for(i in seq_len(nclas)) {
      
      zdots <- c(list(Xr = Xr[Yr == lev[i], , drop = FALSE], Xu = Xu), dots)
      zdots$sigma <- W
    
      fm <- do.call(.dens, zdots) 

      ds[, i] <- fm$fit$fit
        
      }
    
    ### CASE WHERE ALL CLASSES HAVE A DENSITY = 0
    s <- which(rowSums(ds) == 0)
    ds[s, ] <- 1 / nclas 
    ### END
    
    prior <- matrix(rep(prior, m), nrow = m, byrow = TRUE)
      
    z <- ds * prior
    
    posterior <- z / rowSums(z)
  
    z <- apply(posterior, FUN = .findmax, MARGIN = 1) 
    fit <- vapply(z, FUN = function(x) lev[x], FUN.VALUE = "")
    
    colnames(posterior) <- colnames(prior) <- colnames(ds) <- lev
    rownames(posterior) <- rownames(prior) <- rownames(ds) <- rownam.Xu
  
    }
  
  y <- Yu
  r <- as.numeric(y != fit)
  
  y <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, y, stringsAsFactors = FALSE)
  fit <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, fit, stringsAsFactors = FALSE)
  r <- data.frame(rownum = seq_len(m), rownam = rownam.Xu, r)
  names(r)[ncol(r)] <- names(fit)[ncol(fit)] <- names(y)[ncol(y)] <- colnam.Y
  
  list(y = y, fit = fit, r = r, posterior = posterior, ds = ds, prior = prior, 
       ni = ni)

  }
  
  
  