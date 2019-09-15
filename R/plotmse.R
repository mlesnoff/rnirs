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
      namgroup <- paste(as.character(substitute(group)), collapse = "")
      if(is.character(group) & length(group == 1))
        group <- as.factor(z[, group]) else 
            group <- as.factor(group)
      p <- ggplot(data = z, aes(x = z[, "ncomp"], y = z[, j])) #, linetype = group
      p <- p + geom_line(aes(group = group, col = group))
      p <- p + scale_colour_discrete(name = namgroup)
      if(scale.x.cont)
        p <- p + scale_x_continuous(breaks = min(z[, "ncomp"]):max(z[, "ncomp"]))
      p <- p + labs(x = "Nb. components", y = toupper(nam))
      }
  
  p
  
  }
