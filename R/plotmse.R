plotmse <- function(obj, nam = "rmsep", group = NULL, scale.x.cont = TRUE) {
  
  z <- obj
  
  j <- which(nam == colnames(z))
  
  if(is.null(group)) {
    
    p <- ggplot(data = z, aes(x = z[, "ncomp"], y = z[, j]))
    p <- p + geom_line() 
    if(scale.x.cont)
      p <- p + scale_x_continuous(breaks = min(z[, "ncomp"]):max(z[, "ncomp"]))
    p <- p + labs(x = "Nb. components", y = toupper(nam))
    
    } else {
      
      u <- obj[, group]
      if(is.numeric(u) | is.character(u)) u <- as.factor(u) 
      z$group <- u
      
      p <- ggplot(data = z, aes(x = z[, "ncomp"], y = z[, j]))
      p <- p + geom_line(aes(group = group, col = group))
      p <- p + scale_colour_discrete(name = group)
      if(scale.x.cont)
        p <- p + scale_x_continuous(breaks = min(z[, "ncomp"]):max(z[, "ncomp"]))
      p <- p + labs(x = "Nb. components", y = toupper(nam))
      }
  
  p
  
  }
