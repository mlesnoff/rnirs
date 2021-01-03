.plot_scree <- function(x, start = 1, opt = NULL, ...) {
    
    n <- length(x)
    
    index <- seq(start, start + n - 1)
      
    fg <- "grey70"
    col <- "#045a8d"
    xmin <- min(index)
    xmax <- max(index)
    eps <- .8
        
    if(n <= 55)
        labs <- seq(xmin, xmax, by = 2)
    else
        labs <- seq(xmin, xmax, by = 10)
    
    plot(
        index, x, 
        type = "l", col = col, pch = 16,
        xaxt = "n", las = 1, fg = fg, las = 1, 
        xlim = c(xmin - eps, xmax + eps), xaxs = "i",
        ...
        )
    axis(side = 1, at = labs, labels = labs, fg = fg)
    points(index, x, pch = 16, col = col)
    
    #if(!is.null(opt))
    #    points(zncomp[zu], zeig[zu], pch = 16, col = "grey", cex = 1.2)
        
    }
