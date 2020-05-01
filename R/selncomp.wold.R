selncomp.wold <- function(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "integral", "smooth"), correct = TRUE, digits = 3,
  plot = TRUE, ...
  ) {
  
  typ <- match.arg(typ)
  
  obj <- obj[order(obj$ncomp), ]
  
  ncompmin <- min(obj$ncomp)
  ncompmax <- max(obj$ncomp)
  ncomp <- ncompmax - ncompmin + 1
  
  obj$valref <- obj$val <- obj[, nam]
  if(typ == "integral")
    obj$valref <- cumsum(obj$val)
  if(typ == "smooth")
    obj$valref <- lowess(obj$ncomp, obj$val, ...)$y
  
  res <- obj[, c("ncomp", "val", "valref")]
  res$valref1 <- res$valref[1:ncomp]
  res$valref2 <- c(res$valref[2:ncomp], NA)
  res$r <- 1 - res$valref2 / res$valref1
  if(typ == "integral")
    res$r <- -res$r
  
  opt <- res$ncomp[res$val == min(res$val)][1]
  sel <- res$ncomp[res$r < alpha][1]
  if(correct)
    sel <- min(opt, sel)

  res$r <- round(res$r, digits = digits)

  if(plot) {
    
    par(mfrow = c(1, 2))
    plotmse(obj, nam = nam)
    abline(v = c(opt, sel), col = c("grey", "blue"), lty = 2)
    if(typ == "smooth") {
      
      lines(obj[, c("ncomp", "valref")], typ = "l", col = "red")
      
      legend("topright", legend = c("Raw", "Smoothed"),
        box.col = "grey70", ncol = 1,
        col = c("#045a8d", "red"), lty = 1, xjust = 1, yjust = 1)
      
      }
      
    z <- res[1:(nrow(res) - 1), c("ncomp", "r")]
    plot(z, typ = "l", col = "grey40",
      xlab = "Nb. components", ylab = "% Gain")
    points(z, col = "grey40", pch = 16)
    abline(h = alpha, col = "blue", lty = 2)
    
    par(mfrow = c(1, 1))
    
    }

  list(res = res, sel = sel, opt = opt)
    
  }
