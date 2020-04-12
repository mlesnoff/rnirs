wdist <- function(d, h = 2, c = 3) {
  
  d <- c(d)
  
  w <- ifelse(d <  median(d) + c * mad(d), 1 / exp(d / (h * mad(d))), 0)
  w <- w / max(w)
  
  w[is.na(w) | is.nan(w)] <- 1
    
  w
  
  }





