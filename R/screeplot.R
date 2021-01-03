screeplot <- function(fm, ncomp = NULL,
                      scree = c("log", "eig", "split"),
                      ...) {
    
    scree <- match.arg(scree)
    
    if(is.null(ncomp))
        ncomp <- dim(fm$T)[2]
    else
      ncomp <- min(ncomp, dim(fm$T)[2])
    zncomp <- seq_len(ncomp)
    
    if(is.null(fm$T))
        names(fm)[which(names(fm) == "Tr")] <- "T"
    
    eig <- fm$eig
    if(is.null(eig))
        ## PLS case
        eig <- colSums(fm$P * fm$P) * fm$TT
    eig <- eig[zncomp]
    
    if(scree == "log")
        res <- plotsl(log(eig), 
                      xlab = "Nb. components", ylab = "Eig.", 
                      main = "log-scale", ...)
        
    if(scree == "eig")
        res <- plotsl(eig, 
                      xlab = "Nb. components", ylab = "Eig.", 
                      ...)
    
    if(scree == "split") {
      
        zT <- fm$T[, zncomp, drop = FALSE]
        plotsp(zT, xlab = "Nb. components", ylab = "Score", ...)
        lines(sqrt(eig), col = "red", lwd = 2)
        abline(h = 0, col = "grey")
        
        res <- NULL
            
        }
    
    invisible(res)
    
    }


