selncomp.wold <- function(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "integral", "smooth"), correct = TRUE, digits = 3,
  plot = TRUE, title = NULL, ...
  ) {
  
  typ <- match.arg(typ)
  
  obj <- obj[order(obj$ncomp), ]
  
  ncompmin <- min(obj$ncomp)
  ncompmax <- max(obj$ncomp)
  ncomp <- ncompmax - ncompmin + 1
  
  obj$valref <- obj$val <- obj[, nam]
  if(typ == "integral") obj$valref <- cumsum(obj$val)
  if(typ == "smooth") obj$valref <- lowess(obj$ncomp, obj$val, ...)$y
  
  res <- obj[, c("ncomp", "val", "valref")]
  res$valref1 <- res$valref[1:ncomp]
  res$valref2 <- c(res$valref[2:ncomp], NA)
  res$r <- 1 - res$valref2 / res$valref1
  if(typ == "integral") res$r <- - res$r
  
  opt <- res$ncomp[res$val == min(res$val)][1]
  sel <- res$ncomp[res$r < alpha][1]
  if(correct) sel <- min(opt, sel)

  res$r <- round(res$r, digits = digits)

  p <- NULL
  if(plot) {
    
    p <- plotmse(obj, nam = nam, scale.x.cont = FALSE) + geom_point()
    if(typ == "smooth") p <- p + geom_line(aes(x = obj$ncomp, y = obj$valref), col = "red")
    p <- p + geom_vline(xintercept = opt, col = "grey", lty = 2)
    p <- p + geom_vline(xintercept = sel, col = "blue", lty = 2)
    if(!is.null(title)) p <- p + ggtitle(title)
    p1 <- p

    z <- res[1:(nrow(res) - 1), ]
    p <- ggplot(data = z, aes(x = z[, "ncomp"], y = z[, "r"]))
    p <- p + geom_point() + geom_line()
    p <- p + geom_hline(yintercept = alpha, col = "blue", lty = 2)
    if(!is.null(title)) p <- p + ggtitle(title)
    p2 <- p + labs(x = "Nb. components", y = "% Gain")
    
    grid.arrange(p1, p2, ncol = 2)
    p <- list(p1, p2)
    
    }

  list(res = res, sel = sel, opt = opt, p = p)
    
  }
