splitpar <- function(x, lim = x * c(.1, 10), n = 20) {
  
  z1 <- seq(lim[1], lim[2], length.out = round(n / 2))
  z2 <- exp(seq(log(lim[1]), log(lim[2]), length.out = round(n / 2)))
  z2 <- z2[-c(1, length(z2))]
  z3 <- seq(.8, 1.2, by = .1) * x
  z3 <- z3[z3 >= lim[1] & z3 <= lim[2]]
  
  sort(unique(round(c(z1, z2, z3), digits = 10)))
  
  }