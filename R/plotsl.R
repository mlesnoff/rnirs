plotsl <- function(
    x,
    start = 1,
    plot = c("slope", "diff", "none"),
    xlab = "Index", ylab = "Value",
    ...
    ) {
    
    plot <- match.arg(plot)
    
    n <- length(x)
    
    index <- seq(start, start + n - 1)
    
    zdiff <- diff(x)
    sl <- zdiff / abs(x[-length(x)])
  
    zdiff <- c(zdiff, NA)
    sl <- c(sl, NA)
    
    if(plot %in% c("slope", "diff")) {
      
        fg <- "grey70"
        col <- "#045a8d"
        xmin <- min(index)
        xmax <- max(index)
        eps <- .8
        
        if(n <= 55)
            labs <- seq(xmin, xmax, by = 2)
        else
            labs <- seq(xmin, xmax, by = 10)
  
        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))

        .plot_scree(x,
                    xlab = xlab, ylab = ylab, ...)
    
        if(plot == "slope") {
            .plot_scree(sl,
                        xlab = xlab, ylab = ylab, main = "Slope")
            abline(h = median(sl, na.rm = TRUE), col = "grey")
          }

        if(plot == "diff") {
            .plot_scree(zdiff,
                        xlab = xlab, ylab = ylab, main = "Difference")
            abline(h = median(zdiff, na.rm = TRUE), col = "grey")
            }

        par(oldpar)
      
        }
    
    list(index = index, x = x, diff = zdiff, slope = sl)
        
    }
