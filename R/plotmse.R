plotmse <- function(
    obj, 
    namx = "ncomp", nam = "msep", 
    group = NULL,
    col = NULL, legend = TRUE, legend.title = NULL, ncol = 1, 
    ...
    ) {
    
    dots <- list(...)

    obj <- obj[, c(namx, nam)]
    
    if(is.null(dots$lwd))
        dots$lwd <- 1.8
    
    fg <- "grey70"
        
    plot(
        obj[, seq_len(2)], 
        type = "n",
        xaxt = "n", las = 1, fg = fg,
        ...
        )
    abline(h = min(obj[, nam]), col = "grey")
                 
    if(namx == "ncomp") {
        labs <- seq(0, max(obj$ncomp))
        labs[1 + seq(1, max(obj$ncomp), by = 2)] <- NA
        axis(side = 1, at = labs, labels = labs, fg = fg)
        }
    else
        axis(side = 1)
            
    if(is.null(group)) {
        
        if(is.null(col))
            col <- "#045a8d"

        do.call(lines, 
                        c(list(x = obj[, 1], y = obj[, 2], col = col), dots))
    
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
            col <- palette.colors(n = nlev, palette = "ggplot2", recycle = TRUE)
     
        for(i in seq_len(nlev)) {

            do.call(lines,
                c(list(x = obj[group == levs[i], 1], y = obj[group == levs[i], 2], 
                    col = col[i]), dots))
            
            }
        
        if(legend) {
            
            if(is.null(legend.title))
                legend.title <- "Group"
            
            legend("topright", legend = levs,
                box.col = fg, ncol = ncol,
                col = col, lty = 1, xjust = 1, yjust = 1,
                title = legend.title)
            }
        
        }
        
    }
