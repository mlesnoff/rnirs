lscordis <- function(fm, alpha = .01) {
    
    typcut <- match.arg(typcut)
    
    fm <- fm$fm
    
    nmod <- length(fm)
    nam <- names(fm)
    du <- dr <- vector("list", length = nmod)
    
    for(i in seq_len(nmod)) {
        
        z <- scordis(fm[[i]], alpha = alpha)
        
        z$dr$rownum <- fm[[i]]$nn
        
        z$dr$modnum <- rep(i, nrow(z$dr))
        z$du$modnum <- i
            
        dr[[i]] <- z$dr
        du[[i]] <- z$du
        
        }
        
    dr <- setDF(rbindlist(dr))
    du <- setDF(rbindlist(du))
    
    list(dr = dr, du = du)

    }





