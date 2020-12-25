odis <- function(fm, Xr, Xu = NULL, 
                                 ncomp = NULL, typcut = c("param", "mad", "boxplot")) {
    
    if(is.null(fm$Tr))
        names(fm)[which(names(fm) == "T")] <- "Tr"
    
    if(is.null(ncomp))
        ncomp <- dim(fm$Tr)[2]
    else 
        ncomp <- min(ncomp, dim(fm$Tr)[2])

    typcut <- match.arg(typcut)
    
    X <- .matrix(Xr)
    n <- dim(X)[1]
    rownam <- row.names(X)
    
    E <- .center(X, fm$xmeans) - tcrossprod(fm$Tr[, seq_len(ncomp), drop = FALSE], 
                                                                                    fm$P[, seq_len(ncomp), drop = FALSE])
    
    ## Same as
    ## E <- Xr - xfit(fm$Tr[, seq_len(ncomp), drop = FALSE],
    ##                                fm$P[, seq_len(ncomp), drop = FALSE], fm$xmeans)
    ## End
    
    d <- sqrt(rowSums(E * E))
    
    cri <- 2.5
    #cri <- 3
    
    cutoff <- switch(
        typcut, 
        param = {
            z <- d^(2/3)
            (median(z) + mad(z) * qnorm(p = .975))^(3/2)
            },
        mad = median(d) + cri * mad(d),
        boxplot = {
            z <- fivenum(d)
            z <- z[4] + 1.5 * diff(z[c(2, 4)])
            max(d[d <= z])
            }
        )    
    
    dstand <- d / cutoff 
    
    dr <- data.frame(rownum = seq_len(n), rownam = rownam, ncomp = rep(ncomp, n), 
        d = d, dstand = dstand)
    rownames(dr) <- seq_len(n)
    
    ### NEW OBSERVATIONS
    
    Eu <- du <- NULL
    if(!is.null(Xu)) {
        
        Xu <- .matrix(Xu)
        m <- dim(Xu)[1]
        rownam <- row.names(Xu)
        
        Tu <- .projscor(fm, Xu)

        E <- .center(Xu, fm$xmeans) - tcrossprod(Tu[, seq_len(ncomp), drop = FALSE], 
                                                                                         fm$P[, seq_len(ncomp), drop = FALSE])
        ## Same as:
        ## E <- Xu - xfit(fm$Tu[, seq_len(ncomp), drop = FALSE],
        ##                            fm$P[, seq_len(ncomp), drop = FALSE], fm$xmeans)
        ## End
     
        d <- sqrt(rowSums(E * E))
        
        dstand <- d / cutoff 
        
        du <- data.frame(rownum = seq_len(m), rownam = rownam, ncomp = rep(ncomp, m), 
            d = d, dstand = dstand)
        rownames(du) <- seq_len(m)
        
        }
    
    ### END

    list(dr = dr, du = du, cutoff = cutoff)

    }