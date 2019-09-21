\name{wkern}
\alias{wkern}
\encoding{latin1}

\title{Kernel weights}

\description{
Calculation of weights from a vector of distances, using a kernel function.

Let \eqn{d} be a vector of distances, \eqn{u = d / max(d)} the normalized distance, and \eqn{h} a scaling factor (scalar). The kernel weights are either \eqn{w = exp(-u / (h * sd(u))} (if argument \code{squared = FALSE}; Kim et al. 2011) or \eqn{w = exp(-u^2 / (h * sd(u))} (if argument \code{squared = TRUE}). In the last case and if \eqn{h = 1}, this is the gaussian kernel function. The function returns the normalized weights \eqn{w / max(w)}.

}

\usage{wkern(d, h = 1, squared = FALSE, trim = TRUE)}

\arguments{

\item{d}{A vector of distances.}

\item{h}{Scaling factor. Lower is \eqn{h}, sharper is the function. See the examples.}

\item{squared}{Logical. If \code{TRUE}, the normalized distance is squared. Default to \code{FALSE}.}

\item{trim}{Logical. If \code{TRUE} (default), the maximal distance used to calculate the normaized distance is trimmed by \eqn{median(d) + 3 * mad(d)}. This robustifies the normalization.}

}

\references{

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighertd partail least squares and statistical wavelength selection. Int. J. Pharma., 412, 269-274.

}

\examples{

library(ggplot2)
library(gridExtra)

d <- 0:15
squared <- FALSE
#squared <- TRUE
w <- wkern(d, h = 1, squared = squared)
plot(d / max(d), w, xlab = "Normalized distance", ylab = "Normalized weight", type = "l") 

d <- 0:15
h <- c(.5, 1, 1.5, 2.5, 5)
for(i in 1:length(h)) {
  w <- wkern(d, h = h[i])
  z <- data.frame(d = d, w = w, h = rep(h[i], length(d)))
  if(i == 1) res <- z else res <- rbind(res, z)
  }
res$u <- res$d / max(res$d)
res$h <- as.factor(res$h)
head(res)
p <- ggplot(data = res, aes(x = u, y = w, col = h))
p <- p + xlab("Normalized distance") + ylab("Normalized weight")
p1 <- p + geom_line(aes()) + ggtitle("Squared = FALSE")
h <- c(.2, .5, 1, 2 , 4)
for(i in 1:length(h)) {
  w <- wkern(d, h = h[i], squared = TRUE)
  z <- data.frame(d = d, w = w, h = rep(h[i], length(d)))
  if(i == 1) res <- z else res <- rbind(res, z)
  }
res$u <- res$d / max(res$d)
res$h <- as.factor(res$h)
head(res)
p <- ggplot(data = res, aes(x = u, y = w, col = h))
p <- p + xlab("Normalized distance") + ylab("Normalized weight")
p2 <- p + geom_line(aes()) + ggtitle("Squared = TRUE")
grid.arrange(p1, p2, ncol = 2)

}

\keyword{datagen}