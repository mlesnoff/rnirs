selncomp.wold <- function(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "integral", "smooth"), correct = TRUE, digits = 3,
  plot = TRUE, ...
  ) {
  
  typ <- match.arg(typ)
  
  obj <- obj[order(obj$ncomp), ]
  
  res <- obj[, c("ncomp", nam)]
  
  z <- obj[, nam]
  if(typ == "integral")
    z <- cumsum(z)
  if(typ == "smooth")
    z <- lowess(obj$ncomp, z, ...)$y
  
  res$valref <- z
  res$diff <- c(rev(diff(rev(z))), NA)
  res$r <- res$diff / abs(z)
  if(typ == "integral")
    res$r <- -res$r
  
  opt <- res$ncomp[res$valref == min(res$valref)][1]
  sel <- res$ncomp[res$r < alpha][1]
  if(correct)
    sel <- min(opt, sel)

  res$r <- round(res$r, digits = digits)

  if(plot) {
    
    par(mfrow = c(1, 2))
    
    plot(res$ncomp, res[, nam], typ = "b", col = "#045a8d",
      las = 1, pch = 16, xlab = "Nb. components", ylab = "Value", main = toupper(nam))
    abline(v = c(opt, sel), col = c("grey", "blue"), lty = 2)
    if(typ == "smooth") {
      
      lines(res[, c("ncomp", "valref")], typ = "l", col = "red")
      
      legend("topright", legend = c("Raw", "Smoothed"),
        box.col = "grey70", ncol = 1,
        col = c("#045a8d", "red"), lty = 1, xjust = 1, yjust = 1)
      
      }
      
    plot(res$ncomp, res$r, typ = "l", col = "grey40",
      xlab = "Nb. components", ylab = "Proportion", main = "Relative Gain")
    points(res$ncomp, res$r, col = "grey40", pch = 16)
    abline(h = alpha, col = "blue", lty = 2)
    
    par(mfrow = c(1, 1))
    
    }

  list(res = res, sel = sel, opt = opt)
    
  }
