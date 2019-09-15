sampclas <- function(X, y, m = 1, meth = c("random")) {
  
  meth <- match.arg(meth)
  
  X <- .matrix(X)
  n <- nrow(X)

  y <- as.factor(y)
  ni <- tabulate(y)
  nclus <- length(ni)
  lev <- as.character(levels(y))
  
  if(length(m) == 1) m <- rep(m, nclus)
  
  z <- data.frame(rownum = 1:n, y = y)
  
  for(i in 1:nclus) {
    
    u <- z[as.numeric(z$y) == i, , drop = FALSE]
    
    if(length(u) == 0) {
      zs <- NULL
      zm <- 0
      }
    else {
      n.u <- nrow(u)
      zm <- m[i]
      if(n.u < zm) zm <- n.u
      if(meth == "random") 
        zs <- sort(sample(u$rownum, size = zm))
      }
    
    if(i == 1) s <- zs else s <- c(s, zs)
    if(i == 1) ns <- zm else ns <- c(ns, zm)
    
    }

  p <- ni / sum(ni) 
  
  tab <- data.frame(y = lev, n = ni, p = p, m = ns)
  tab$psamp <- tab$m / sum(tab$m)
  tab[, c("p", "psamp")] <- round(tab[, c("p", "psamp")], digits = 3)
  
  list(s = s, tab = tab, y = z$y)
  
  }
