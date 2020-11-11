sampclas <- function(x, y = NULL, m = 1, seed = NULL) {
  
  x <- as.integer(c(x))
  
  n <- length(x)
  ni <- tabulate(as.factor(x))
  nclas <- length(ni)
  lev <- sort(unique(x))

  if(length(m) == 1) 
    m <- rep(m, nclas)
  else
    if(length(m) != nclas)
      stop("\n\n  Length of argument 'm' must be =1 or =the number of classes in vector'x'. \n\n") 
  
  s <- list()
  set.seed(seed = seed) 
  for(i in 1:nclas) {
    
    m[i] <- min(m[i], ni[i])
    
    zs <- which(x == lev[i])
    
    
    if(is.null(y)) {
      s[[i]] <- sort(sample(zs, size = m[i], replace = FALSE))
      } 
    else {
      zy <- y[zs]
      ## "order(y) = 3 7 etc." means: the 1st lower value is the component 3 of the vector,
      ## the 2nd lower value is the component 7 of the vector,
      id <- order(zy)
      u <- round(seq(1, ni[i], length = m[i]))
      s[[i]] <- sort(zs[id[u]])
      }
  
    }
  set.seed(seed = NULL)
  
  names(s) <- lev

  s
  
  }
