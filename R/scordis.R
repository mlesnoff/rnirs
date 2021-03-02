scordis <- function(
    fm, 
    ncomp = NULL, 
    robust = FALSE, alpha = .01
    ) {

    
    if(is.null(fm$Tr))
        names(fm)[which(names(fm) == "T")] <- "Tr"
    
    if(is.null(ncomp))
        ncomp <- dim(fm$Tr)[2]
    else 
        ncomp <- min(ncomp, dim(fm$Tr)[2])
    
    if(fm$T.ortho) {
        tt <- colSums(
            fm$weights * fm$Tr[, seq_len(ncomp), drop = FALSE] * fm$Tr[, seq_len(ncomp), drop = FALSE]
            )
        S <- diag(tt, nrow = ncomp, ncol = ncomp)
        }
    else 
        S <- NULL
    
    res <- dis(fm$Tr[, seq_len(ncomp), drop = FALSE], fm$Tu[, seq_len(ncomp), drop = FALSE], 
                         rep(0, ncomp), "mahalanobis", S)
    
    dr <- res$dr

    d2 <- dr$d^2
    if(!robust) {
        mu <- mean(d2)   
        s2 <- var(d2)
        }
    else{
        mu <- median(d2)
        s2 <- mad(d2)^2
        }
    nu <- 2 * mu^2 / s2
    cutoff <- sqrt(mu / nu * qchisq(1 - alpha, df = nu))
    
    dr$dstand <- dr$d / cutoff
    dr$gh <- dr$d^2 / ncomp
    
    du <- NULL
    if(!is.null(fm$Tu[, seq_len(ncomp), drop = FALSE])) {
        du <- res$du
        du$dstand <- du$d / cutoff
        du$gh <- du$d^2 / ncomp
        }

    list(dr = dr, du = du, cutoff = cutoff)
    
    }


