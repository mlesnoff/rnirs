scordis <- function(fm, 
                                        ncomp = NULL, typcut = c("param", "mad", "boxplot")) {
    
    if(is.null(fm$Tr))
        names(fm)[which(names(fm) == "T")] <- "Tr"
    
    if(is.null(ncomp))
        ncomp <- dim(fm$Tr)[2]
    else 
        ncomp <- min(ncomp, dim(fm$Tr)[2])
    
    typcut <- match.arg(typcut)
    
    if(fm$T.ortho) {
        tt <- colSums(fm$weights * fm$Tr[, seq_len(ncomp), 
                                                                         drop = FALSE] * fm$Tr[, seq_len(ncomp), drop = FALSE])
        S <- diag(tt, nrow = ncomp, ncol = ncomp)
        }
    else 
        S <- NULL
    
    res <- dis(fm$Tr[, seq_len(ncomp), drop = FALSE], fm$Tu[, seq_len(ncomp), drop = FALSE], 
                         rep(0, ncomp), "mahalanobis", S)
    
    dr <- res$dr
    
    cri <- 2.5
    #cri <- 3
    
    d <- dr$d
    cutoff <- switch(
        typcut, 
        param = qchisq(p = .975, df = ncomp)^.5,
        mad = median(d) + cri * mad(d),
        boxplot = {
            z <- fivenum(d)
            z <- z[4] + 1.5 * diff(z[c(2, 4)])
            max(d[d <= z])
            }
        )    

    dr$dstand <- d / cutoff
    dr$gh <- d^2 / ncomp
    
    du <- NULL
    if(!is.null(fm$Tu[, seq_len(ncomp), drop = FALSE])) {
        du <- res$du
        du$dstand <- du$d / cutoff
        du$gh <- du$d^2 / ncomp
        }

    list(dr = dr, du = du, cutoff = cutoff)
    
    }


