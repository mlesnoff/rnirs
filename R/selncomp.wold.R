selncomp.wold <- function(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "integral", "smooth"), 
  correct = TRUE, digits = 3,
  plot = TRUE, rm0 = TRUE, ...
  ) {
  
  typ <- match.arg(typ)
  
  if(is.vector(obj)){
    obj <- data.frame(ncomp = 1:length(obj), y = obj)
    nam <- "y"
    }
  
  obj <- obj[order(obj$ncomp), ]
  res <- obj[, c("ncomp", nam)]
  
  z <- res[, nam]
  if(typ == "integral")
    z <- cumsum(z)
  if(typ == "smooth")
    z <- lowess(obj$ncomp, z, ...)$y
  
  res$val <- z
  res$diff <- c(rev(diff(rev(z))), NA)
  res$r <- res$diff / abs(z)
  if(typ == "integral")
    res$r <- -res$r
  
  opt <- res$ncomp[res$val == min(res$val)][1]
  sel <- res$ncomp[res$r < alpha][1]
  if(correct)
    sel <- min(opt, sel)

  res$r <- round(res$r, digits = digits)

  if(plot) {

    if(rm0)
      zres <- res[res$ncomp >= 1, ]
    else
      zres <- res
    
    fg <- "grey70"
    zncomp <- zres$ncomp
    xmin <- min(zncomp)
    xmax <- max(zncomp)
    
    if(xmax <= 50)
      labs <- seq(0, xmax, by = 2)
    else
      labs <- seq(0, xmax, by = 10)
    
    par(mfrow = c(1, 2))
    
    plot(
      zncomp, zres[, nam], 
      typ = "b", col = "#045a8d", pch = 16,
      xaxt = "n", las = 1, fg = fg, las = 1, 
      xlim = c(0, xmax), xaxs = "i",
      xlab = "Nb. components", ylab = nam, main = "Reference value"
      )
    axis(side = 1, at = labs, labels = labs, fg = fg)
    abline(v = c(opt, sel), col = c("grey", "blue"), lty = 2)
    
    if(typ == "smooth") {
      lines(zres[, c("ncomp", "val")], typ = "l", col = "red")
      legend("topright", legend = c("Raw", "Smoothed"),
        box.col = "grey70", ncol = 1,
        col = c("#045a8d", "red"), lty = 1, xjust = 1, yjust = 1)
      }
      
    ylim <- c(min(0, zres$r, na.rm = TRUE), max(zres$r, na.rm = TRUE))
    plot(
      zres$ncomp, zres$r, 
      typ = "b", pch = 16, col = "grey40",
      xaxt = "n", las = 1, fg = fg, las = 1,
      xlim = c(0, xmax), xaxs = "i", ylim = ylim,
      xlab = "Nb. components", ylab = "R", main = "Relative gain"
      )
    axis(side = 1, at = labs, labels = labs, fg = fg)
    abline(h = alpha, col = "blue", lty = 1)
    
    par(mfrow = c(1, 1))
    
    }

  list(res = res, sel = sel, opt = opt)
    
  }
