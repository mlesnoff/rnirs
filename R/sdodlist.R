sdodlist <- function(fm, ncomp = NULL, ...) {
  
  nmod <- length(fm)
  nam <- names(fm)
  odu <- odr <- sdu <- sdr <- vector("list", length = nmod)
  
  res <- lapply(
    
    1:nmod, function(i) {
      
      z <- sdod(fm[[i]], ncomp = ncomp, ...)
        
      sdr[[i]] <<- z$sdr
      sdu[[i]] <<- z$sdu
      odr[[i]] <<- z$odr
      odu[[i]] <<- z$odu
      
      #odr[[i]]$rownum <- sdr[[i]]$rownum <- nam[[i]]
      #odu[[i]]$rownum <- sdu[[i]]$rownum <- nam[[i]]
        
      odr[[i]]$fm <<- sdr[[i]]$fm <<- nam[[i]]
      odu[[i]]$fm <<- sdu[[i]]$fm <<- nam[[i]]
      
      }
    
    )
    
  sdr <- setDF(rbindlist(sdr))
  sdu <- setDF(rbindlist(sdu))
  odr <- setDF(rbindlist(odr))
  odu <- setDF(rbindlist(odu))
  
  res <- list(sdr = sdr, sdu = sdu, odr = odr, odu = odu)
  
  res
  
  }

