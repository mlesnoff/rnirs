cvfit <- function(X, Y, fun, segm, print = FALSE, ...) {
  
  fun <- match.fun(FUN = fun)

  X <- .matrix(X)
  n <- nrow(X)
  
  if(is.factor(Y) | is.vector(Y)) 
    Y <- .matrix(Y, row = FALSE,  prefix.colnam = "y")
  nvar <- ncol(Y)
  colnam.Y <- colnames(Y)
  if(is.null(colnam.Y)) 
    colnam.Y <- paste("y", seq_len(nvar), sep = "")
  
  nrep <- length(segm)
  
  r <- fit <- y <- vector("list", length = nrep)
  
  for(i in seq_len(nrep)) {
    
    if(print)
      cat("/ rep=", i, " ", sep = "") 
      
    
    listsegm <- segm[[i]]
    nsegm <- length(listsegm)
    
    zr <- zfit <- zy <- vector("list", length = nsegm)
    
    for(j in seq_len(nsegm)) {
      
      s <- sort(listsegm[[j]])
      
      if(print)
        cat("segm=", j, " ", sep = "")
        #cat("\n\n------------------------- Repetition: ", i, "  Segment: ", j,
        #  "\n\nRow numbers of X to predict: \ns =", s,
        #  "\n(The models are fitted on X[-s, ], Y[-s].)\n\n")
      
      fm <- fun(
        X[-s, , drop = FALSE], 
        Y[-s, , drop = FALSE], 
        X[s, , drop = FALSE], 
        Y[s, , drop = FALSE],
        #print = FALSE,
        ...
        )
      
      zy[[j]] <- fm$y
      
      zfit[[j]] <- fm$fit
      zr[[j]] <- fm$r
      
      zr[[j]]$segm <- zfit[[j]]$segm <- zy[[j]]$segm <- rep(j, length(s)) 
      
      }

    y[[i]] <- setDF(rbindlist(zy))
    fit[[i]] <- setDF(rbindlist(zfit))
    r[[i]] <- setDF(rbindlist(zr))
    
    m <- nrow(y[[i]])
    r[[i]]$rep <- fit[[i]]$rep <- y[[i]]$rep <- rep(i, m) 
  
    }
  
  if(print)
    cat("/ End. \n\n")
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  u <- which(names(y) %in% c("segm", "rep"))
  u <- c(u, (seq_len(ncol(y)))[-u])
  y <- y[, u] ; fit <- fit[, u] ; r <- r[, u]

  list(y = y, fit = fit, r = r)
  
  }
