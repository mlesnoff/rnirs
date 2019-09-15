fitcv <- function(X, Y, fun, segm, print = FALSE, ...) {
  
  fun <- match.fun(FUN = fun)

  X <- .matrix(X)
  n <- nrow(X)
  
  if(is.vector(Y)) Y <- .matrix(Y, row = FALSE)
  nvar <- ncol(Y)
  colnam.Y <- colnames(Y)
  if(is.null(colnam.Y)) colnam.Y <- paste("y", 1:nvar, sep = "")
  
  nrep <- length(segm)
  
  r <- fit <- y <- vector("list", length = nrep)
  
  for(i in 1:nrep) {
    
    listsegm <- segm[[i]]
    nsegm <- length(listsegm)
    
    zr <- zfit <- zy <- vector("list", length = nsegm)
    
    for(j in 1:nsegm) {
      
      s <- sort(listsegm[[j]])
      
      if(print)
        cat("\n\n------------------------- Repetition: ", i, "  Segment: ", j,
          "\n\nRow numbers of X to predict: \ns =", s,
          "\n(The models are fitted on X[-s, ], Y[-s].)\n\n")
      
      zXr <- X[-s, , drop = FALSE]
      zYr <- Y[-s, , drop = FALSE]
      
      zXu <- X[s, , drop = FALSE]
      zYu <- Y[s, , drop = FALSE]
      
      fm <- fun(zXr, zYr, zXu, zYu, ...)
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
  
  y <- setDF(rbindlist(y))
  fit <- setDF(rbindlist(fit))
  r <- setDF(rbindlist(r))
  
  u <- which(names(y) %in% c("segm", "rep"))
  u <- c(u, (1:ncol(y))[-u])
  y <- y[, u] ; fit <- fit[, u] ; r <- r[, u]

  list(y = y, fit = fit, r = r)
  
  }
