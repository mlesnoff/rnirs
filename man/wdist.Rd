\name{wdist}
\alias{wdist}
\encoding{latin1}

\title{Weights for distances}

\description{
Calculation of weights from a vector of distances, using a (decreasing) inverse exponential function.
}

\usage{wdist(d, h = 2, cri = 3)}

\arguments{

\item{d}{A vector of distances.}

\item{h}{A positive scalar (scaling factor). Lower is \eqn{h}, sharper is the function. See Details and the examples.}

\item{cri}{A positive scalar used for defining outliers in the distances vector. See Details.}

}

\details{
Let \eqn{d} be a vector of distances. Preliminary weights are calculated by \eqn{w = exp(-d / (h * mad(d)))}, where \eqn{h} is a scalar > 0 (scale factor). In addition, the weights of the distances higher than \eqn{median(d) + c * mad(d)}, where \eqn{c} is a scalar > 0, are set to zero.  Finally, the weights are "normalized" between 0 and 1 by \eqn{w = w / max(w)}.
}

\examples{

library(ggplot2)

x1 <- sqrt(rchisq(n = 100, df = 10))
x2 <- sqrt(rchisq(n = 10, df = 40))
d <- c(x1, x2)
h <- 2 ; cri <- 3
w <- wdist(d, h = h, cri = cri)
par(mfrow = c(2, 2))
plot(d)
hist(d, n = 50)
plot(w, ylim = c(0, 1)) ; abline(h = 1, lty = 2)
plot(d, w, ylim = c(0, 1)) ; abline(h = 1, lty = 2)
par(mfrow = c(1, 1))

d <- 0:15
h <- c(.5, 1, 1.5, 2.5, 5, 10, Inf)
for(i in 1:length(h)) {
  w <- wdist(d, h = h[i])
  z <- data.frame(d = d, w = w, h = rep(h[i], length(d)))
  if(i == 1) res <- z else res <- rbind(res, z)
  }
res$h <- as.factor(res$h)
headm(res)
p <- ggplot(data = res, aes(x = d, y = w, col = h))
p <- p + xlab("Distance") + ylab("Weight")
p <- p + geom_line(aes())
p

}

\keyword{datagen}