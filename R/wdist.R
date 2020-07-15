wdist <- function(d, h = 2, cri = 3) {
  
  d <- c(d)
  #d <- c(d)^2

  w <- ifelse(d <  median(d) + cri * mad(d), exp(-d / (h * mad(d))), 0)
  w <- w / max(w)
  
  w[is.na(w) | is.nan(w)] <- 1
    
  w
  
  }





