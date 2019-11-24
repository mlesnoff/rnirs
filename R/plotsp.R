plotsp <- function(
  X,
  blocks = NULL,
  col = NULL,
  col.low = "#ece7f2", col.high = "#045a8d",
  xlim = NULL, ylim = NULL,
  xlab = "x-value", ylab = "y-value", main = NULL,
  add = FALSE,
  text = FALSE,
  ...
  ) {

  X <- .matrix(X, prefix.colnam = "")
  n <- nrow(X)
  p <- ncol(X)
  
  colnam <- suppressWarnings(as.numeric(colnames(X)))
  if(sum(is.na(colnam)) > 0) 
    colnam <- 1:p  
  
  if(n <= 3) col.high <- col.low <- "#045a8d"
  
  if(is.null(col)) {
    u <- seq(0, 1, length.out = n)
    col <- seq_gradient_pal(low = col.low, high = col.high)(u)
    } 
  else
    col <- rep(col, n)
  
  if(is.null(xlim)) 
    {u <- 0.03 ; xlim <- c((1 - u) * min(colnam), (1 + u) * max(colnam))} 
  
  if(is.null(ylim)) 
    {u <- 0.05 ; ylim <- c((1 - u) * min(X), (1 + u) * max(X))} 
 
  .flines <- function(X, colnam, col, text = FALSE, ...) {
    m <- nrow(X)
    for(i in 1:m) {
      lines(x = colnam, y = X[i, ], col = col[i], ...)
      if(text) {
        z <- p # round(.99 * p)
        text(
          x = 1.02 * colnam[z], y = X[i, z],
          labels = row.names(X)[i], 
          col = col[i], 
          cex = .8
          )
        }
      }
    }

  if(!add) {
    plot(
      x = colnam, y = X[1, ], type = "n",
      xlim = xlim, ylim = ylim,
      xlab = xlab, ylab = ylab,
      main = main
      )
    abline(h = 0, lty = 4, col = "grey50")
    }
  
  if(is.null(blocks))
      .flines(X, colnam, col, text, ...) 
  
  else {
    
    k <- length(blocks)
    for(i in 1:k) {
      
      ztext <- FALSE
      if(i == k)
        ztext <- text 
      
      .flines(X[, blocks[[i]], drop = FALSE], colnam[blocks[[i]]], col, ztext, ...)
      
      }
    
    }
  
  }

