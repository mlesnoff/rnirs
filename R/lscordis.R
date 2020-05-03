lscordis <- function(fm) {
  
  fm <- fm$fm
  
  nmod <- length(fm)
  nam <- names(fm)
  du <- dr <- vector("list", length = nmod)
  
  for(i in 1:nmod) {
    
    z <- scordis(fm[[i]])
    
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





