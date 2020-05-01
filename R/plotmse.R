plotmse <- function(obj, nam = "rmsep", group = NULL,  col = NULL,
  legend = TRUE, legend.title = NULL, ncol = 1, lwd = 1.8, ...) {
  
  obj <- obj[, c("ncomp", nam)]
  
  fg <- "grey70"
    
  plot(obj[, 1:2], 
    xlab = "Nb. components", ylab = toupper(nam),
    type = "n",
    xaxt = "n", las = 1, fg = fg,
    ...
    )
  labs <- u <- 0:max(z$ncomp)
  labs[1 + seq(1, max(z$ncomp), by = 2)] <- NA
  axis(side = 1, at = u, labels = labs, fg = fg) 
      
  if(is.null(group)) {
    
    if(is.null(col))
      col <- "#045a8d"

    lines(obj[, 1:2], col = col, lwd = lwd)
    
    }
  
  else {
  
    if(!is.factor(group))
      group <- as.factor(as.character(group))
    
    levs <- levels(group)
    nlev <- length(levs)
    
    if(!is.null(col)){
      if(length(col) == 1)
        col <- rep(col, nlev)
      }
    else
      #col <- 1:nlev #rep("grey70", nlev)
      col <- palette.colors(n = nlev, palette = "ggplot2", recycle = TRUE)
      #col <- heat.colors(n = nlev, alpha = 1, rev = FALSE)
      #col <- terrain.colors(n = nlev, alpha = 1, rev = FALSE)
   
    for(i in 1:nlev) {
      lines(obj[group == levs[i], 1:2], col = col[i], lwd = lwd)
      }
    
    if(legend) {
      
      if(is.null(legend.title))
        legend.title <- "Group"
      
      legend("topright", legend = levs,
        box.col = fg, ncol = ncol,
        #text.width = 2 * max(strwidth(group)),      
        #text.width = strwidth("1,000,00000000"),
        col = col, lty = 1, xjust = 1, yjust = 1,
        title = legend.title)
      }
    
    }
    
  }
