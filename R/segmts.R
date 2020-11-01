segmts <- function(n, y = NULL, m, nrep = 1, seed = NULL) {
  
  segm <- vector("list", length = nrep)
  names(segm) <- paste("rep", 1:nrep, sep = "")
  
  n <- round(n)
  m <- round(m)
  
  zn <- n
  if(!is.null(y)) {
    if(length(y) != n) stop("y must be of size n")
    yagg <- unique(y)
    zn <- length(yagg)
    }

  set.seed(seed = seed)
  
  for(i in 1:nrep) {
    
    z <- sample(1:zn, size = m, replace = FALSE)
    z <- list(z)
    names(z) <- "segm1"
    
    segm[[i]] <- z
    
    }
  
  if(!is.null(y)) {
    vecn <- 1:n
    zsegm <- segm
    for(i in 1:nrep) {
      u <- segm[[i]][[1]]
      v <- which(y %in% yagg[u])
      zsegm[[i]][[1]] <- v
      }
    segm <- zsegm  
    }

  set.seed(seed = NULL)

  segm
  
  }

