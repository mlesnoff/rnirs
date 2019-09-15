plotxy <- function(X, label = FALSE, group = NULL, 
  origin = colMeans(X[, 1:2]), circle = FALSE, ellipse = FALSE,
  alpha = 1/5, pal = NULL, labpal = NULL, ...) {
  
  X <- as.data.frame(X[, 1:2])
  
  if(!is.null(group) & !is.factor(group)) group <- as.factor(group)

  if(is.null(group)) {
    
    p <- ggplot() 
    
    if(!label)
      p <- p + geom_point(data = X, aes(X[, 1], X[, 2]),
        alpha = alpha, ...)
    else
      p <- p + geom_label(data = X, aes(X[, 1], X[, 2]),
        label = rownames(X), label.size = 1, ...)
    
    if(circle) {
      z <- seq(-pi, pi, length = 300)
      z <- data.frame(x = sin(z), y = cos(z))
      p <- p + geom_path(data = z, aes(z[, 1], z[, 2]), inherit.aes = FALSE) 
      p <- p + coord_fixed()
      }
      
   }
  
  if(!is.null(group)) {
    
    nlev <- length(unique(group))
    
    if(is.null(pal)) {palette("default") ; pal <- 1:nlev}
    if(is.null(labpal)) labpal <- "Group"

    p <- ggplot()
    
    if(!label)
      p <- p + geom_point(data = X, aes(X[, 1], X[, 2], col = group), ...)
    else
      p <- p + geom_label(data = X, aes(X[, 1], X[, 2], col = group),
        label = rownames(X), label.size = 1, ...)
    
    if(length(pal) == 1)
      p <- p + scale_colour_brewer(palette = pal, name = labpal)
    else
      p <- p + scale_colour_manual(values = pal, name = labpal)
    
    if(ellipse) 
      p <- p + stat_ellipse(data = X, aes(X[, 1], X[, 2], col = group),
        type = "norm", level = 0.95)
    
    }
    
    
  if(!is.null(origin)) {
    p <- p + geom_hline(yintercept = origin[1], col = "gray")
    p <- p + geom_vline(xintercept = origin[2], col = "gray")
    }
  p <- p + xlab(colnames(X)[1]) + ylab(colnames(X)[2])
  
  p
  
  }
