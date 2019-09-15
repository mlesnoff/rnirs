plotsp <- function(
  X,
  rownum = NULL,
  step = NULL,
  ranges = NULL,
  col = NULL,
  col.low = "#ece7f2",
  col.high = "#045a8d",
  xlim = NULL, ylim = NULL,
  xlab = "x-value", ylab = "y-value", main = NULL,
  add = FALSE,
  text = FALSE,
  ...
  ) {

  X <- .matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  if(is.null(colnames(X))) colnames(X) <- 1:p  
  
  if(sum(is.na(suppressWarnings(as.numeric(colnames(X))))) > 0) colnames(X) <- 1:p  
  
  w <- as.numeric(colnames(X))
  
  if(is.null(rownum)) rownum <- 1:n
  
  if(!is.null(step)) X <- X[, seq(1, p, by = step)]
  
  if(is.null(col)) {
    u <- seq(0, 1, length.out = n)
    col <- seq_gradient_pal(low = col.low, high = col.high)(u)
    }
  
  if(!is.null(col)) col <- rep(col, n)
  
  if(is.null(xlim)) {u <- 0.03 ; xlim <- c((1 - u) * min(w), (1 + u) * max(w))} else xlim <- xlim
  if(is.null(ylim)) {u <- 0.05 ; ylim <- c((1 - u) * min(X), (1 + u) * max(X))} else ylim <- ylim
 
  .flines <- function(X, w, col, text = FALSE, ...) {
    m <- nrow(X)
    for(i in 1:m) {
      lines(x = w, y = as.vector(X[i, ]), col = col[i], ...)
      if(text) {
        z <- round(.99 * ncol(X))
        text(x = w[z], y = X[i, z], labels = row.names(X)[i], col = col[i], cex = .8)
        }
      }
    }

  if(!add) {
    plot(
      x = w, y = X[1, ], type = "n",
      xlim = xlim, ylim = ylim,
      xlab = xlab, ylab = ylab,
      main = main
      )
    abline(h = 0, lty = 4, col = "grey50")
    }
  
  x <- X[rownum, , drop = FALSE]
  col <- col[rownum]
  
  if(is.null(ranges))
      .flines(X = x, w = w, col = col, text = text, ...) 
    else {
      k <- length(ranges)
      xlist <- selw(x, ranges = ranges)$Xlist
      for(i in 1:k) {
        z <- xlist[[i]]
        if(ncol(z) > 0) {
          zw <- as.numeric(colnames(z))
          if(i == k) ztext <- text else ztext <- FALSE 
          .flines(X = z, w = zw, col = col, text = ztext, ...)
          }
        }
    }
  
  }



