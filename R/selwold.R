selwold <- function(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "smooth", "integral"), 
  correct = TRUE, digits = 3,
  plot = TRUE, ...
  ) {
  
  typ <- match.arg(typ)
  
  if(is.vector(obj)){
    obj <- data.frame(ncomp = 1:length(obj), y = obj)
    nam <- "y"
    }

  res <- obj[order(obj$ncomp), c("ncomp", nam)]
  
  zncomp <- res$ncomp
  r <- res[, nam]
  
  ## val = Value on which are calculated diff and R
  val <- switch(
    typ,
    raw = r,
    smooth = lowess(zncomp, r, ...)$y,
    integral = cumsum(r)
    )
  
  zdiff <- -diff(val)
  
  R <- zdiff / abs(val[-length(val)])
  if(typ == "integral")
    R <- -R
  
  zdiff <- c(zdiff, NA)
  R <- c(R, NA)

  res$val <- val
  res$diff <- zdiff
  res$R <- R
  
  opt <- res$ncomp[r == min(r)][1]
  sel <- res$ncomp[R < alpha][1]
  if(correct)
    sel <- min(opt, sel)

  res$R <- round(res$R, digits = digits)

  if(plot) {
    
    fg <- "grey70"
    xmin <- min(zncomp)
    xmax <- max(zncomp)
    col <- "#045a8d"
    eps <- .8
    
    n <- nrow(res)
    
    if(n <= 55)
      labs <- seq(xmin, xmax, by = 2)
    else
      labs <- seq(xmin, xmax, by = 10)
    
    par(mfrow = c(1, 2))
    
    plot(
      zncomp, r, 
      typ = "l", col = col, pch = 16,
      xaxt = "n", las = 1, fg = fg, las = 1, 
      xlim = c(xmin - eps, xmax + eps), xaxs = "i",
      xlab = "Nb. components", ylab = "Value", main = toupper(nam)
      )
    points(zncomp, r, pch = 16, col = col)
    axis(side = 1, at = labs, labels = labs, fg = fg)
    abline(v = c(opt, sel), col = c("grey", "blue"), lty = 2)
    
    if(typ == "smooth") {
      lines(zncomp, val, typ = "l", col = "red")
      legend("topright", legend = c("Raw", "Smoothed"),
        box.col = "grey70", ncol = 1,
        col = c("#045a8d", "red"), lty = 1, xjust = 1, yjust = 1)
      }
      
    plot(
      zncomp, R, 
      typ = "l", pch = 16, col = col,
      xaxt = "n", las = 1, fg = fg, las = 1,
      xlim = c(xmin - eps, xmax - 1 + eps), xaxs = "i",
      xlab = "Nb. components", ylab = "R", main = "Relative gain"
      )
    points(zncomp, R, pch = 16, col = col)
    axis(side = 1, at = labs, labels = labs, fg = fg)
    abline(h = alpha, col = "blue", lty = 2)
    
    par(mfrow = c(1, 1))
    
    }

  list(res = res, sel = sel, opt = opt)
    
  }
