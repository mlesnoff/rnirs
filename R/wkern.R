wkern <- function(d, h = 1, squared = FALSE, trim = TRUE) {
  
  d <- c(d)
  
  if(trim) {
    cut <- median(d) + 3 * mad(d)
    d[d > cut] <- cut
    }

  u <- d / max(d) # = normalized distance
  
  if(length(u) > 1) sigma <- sd(u) else sigma <- 1

  if(squared) u <- u^2
  
  w <- exp(- u / (h * sigma))
  
  w <- w / max(w) # = normalized weight
  
  w[is.nan(w)] <- 1
  
  w
  
  }





