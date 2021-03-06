selwold <- function(
    r, start = 0, 
    type = c("raw", "smooth", "integral"), 
    alpha = .01, digits = 3,
    plot = c("R", "diff", "none"),
    xlab = "Index", ylab = "Value", main = "r",
    ...
    ) {
    
    type <- match.arg(type)
    plot <- match.arg(plot)
    
    n <- length(r)
    
    zindex <- seq(start, start + n - 1)
    
    ## val = Value on which are calculated diff and R
    val <- switch(
        type,
        raw = r,
        smooth = lowess(zindex, r, ...)$y,
        integral = cumsum(r + abs(min(r)))
        )
    
    zdiff <- -diff(val)
    R <- zdiff / abs(val[-length(val)])
    if(type == "integral")
        R <- -R
    
    zdiff <- c(zdiff, NA)
    R <- c(R, NA)

    opt <- zindex[r == min(r)][1]
    sel <- zindex[R < alpha][1]
    if(is.na(sel))
        sel <- opt
    sel <- min(opt, sel)
    
    res <- data.frame(index = zindex, r = r)
    res$val <- val
    res$diff <- -zdiff
    res$R <- round(R, digits = digits)
    row.names(res) <- seq_len(n)

    if(plot %in% c("R", "diff")) {
        
        fg <- "grey70"
        col <- "#045a8d"
        xmin <- min(zindex)
        xmax <- max(zindex)
        eps <- .8
        
        if(n <= 55)
            labs <- seq(xmin, xmax, by = 2)
        else
            labs <- seq(xmin, xmax, by = 10)
        
        oldpar <- par(mfrow = c(1, 1))
        par(mfrow = c(1, 2))

        plot(
            zindex, r, 
            type = "l", col = col, pch = 16,
            xaxt = "n", las = 1, fg = fg, las = 1, 
            xlim = c(xmin - eps, xmax + eps), xaxs = "i",
            xlab = xlab, ylab = ylab, main = main
            )
        points(zindex, r, pch = 16, col = col)
        if(plot == "R")
            if(sel < opt)
                points(seq(sel + 1, opt), r[zindex %in% seq(sel + 1, opt)], 
                             pch = 16, col = "grey", cex = 1.2)
        points(opt, r[zindex == opt], pch = 16, col = "red", cex = 1.2)
        axis(side = 1, at = labs, labels = labs, fg = fg)
        abline(h = min(r), col = "grey")
        on.exit(par(oldpar))
        
        if(type == "smooth") {
            lines(zindex, val, type = "l", col = "red")
            legend("topright", legend = c("Raw", "Smoothed"),
                box.col = "grey70", ncol = 1,
                col = c("#045a8d", "red"), lty = 1, xjust = 1, yjust = 1)
            }
        
        if(plot == "R") {
            plot(
                zindex, R, 
                type = "l", pch = 16, col = col,
                xaxt = "n", las = 1, fg = fg, las = 1,
                xlim = c(xmin - eps, xmax - 1 + eps), xaxs = "i",
                xlab = xlab, ylab = "R", main = "Relative gain"
                )
            points(zindex, R, pch = 16, col = col)
            axis(side = 1, at = labs, labels = labs, fg = fg)
            abline(h = c(0, alpha), col = c("grey", "blue"), lty = seq_len(2))
            on.exit(par(oldpar))
            }
        
        if(plot == "diff") {
            plot(
                zindex, -zdiff, 
                type = "l", pch = 16, col = col,
                xaxt = "n", las = 1, fg = fg, las = 1,
                xlim = c(xmin - eps, xmax - 1 + eps), xaxs = "i",
                xlab = xlab, ylab = "diff", main = "Difference"
                )
            points(zindex, -zdiff, pch = 16, col = col)
            axis(side = 1, at = labs, labels = labs, fg = fg)
            abline(h = median(-zdiff, na.rm = TRUE), col = "grey")
            on.exit(par(oldpar))
            }
        
        par(oldpar)
        
        }

    list(res = res, opt = opt, sel = sel)
        
    }
