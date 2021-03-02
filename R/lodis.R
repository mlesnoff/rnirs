lodis <- function(fm, Xr, Xu, alpha = .01) {
    
    typcut <- match.arg(typcut)
    
    fm <- fm$fm
    
    Xr <- .matrix(Xr)
    Xu <- .matrix(Xu)
    
    m <- nrow(Xu)
    
    rownam <- row.names(Xr)
    
    nmod <- length(fm)
    nam <- names(fm)
    du <- dr <- vector("list", length = nmod)
    
    j <- 1
    for(i in seq_len(nmod)) {
        
        s <- fm[[i]]$nn
        
        z <- odis(fm[[i]], 
            Xr[s, , drop = FALSE], Xu[j, , drop = FALSE], alpha = .01)
        
        z$dr$rownum <- s
        z$dr$rownam <- rownam[s]
        
        z$dr$modnum <- rep(i, nrow(z$dr))
        z$du$modnum <- i
        
        j <- j + 1
        if(j > m)
            j <- 1
            
        dr[[i]] <- z$dr
        du[[i]] <- z$du
        
        }
        
    dr <- setDF(rbindlist(dr))
    du <- setDF(rbindlist(du))
    
    list(dr = dr, du = du)

    }





