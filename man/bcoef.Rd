\name{bcoef}
\alias{bcoef}
\encoding{latin1}

\title{b-cefficients for a PLS model}

\description{

Function \code{bcoef} returns the "b-coefficients" (= regression coefficients for the \eqn{X}-matrix), including the intercept, from a PLS or PLSR model.

}

\usage{

bcoef(fm)

}

\arguments{

\item{fm}{Output  of functions \code{\link{pls}} or \code{\link{plsr}}, or of the PLS algorithm functions.}

}


\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

fm <- pls(X, Y, ncomp = 3)
#fm <- plsr(X, Y, X, ncomp = 3)
#fm <- pls.kernel(X, Y, ncomp = 3)
bcoef(fm)

fm <- pls(X, Y[, 1], ncomp = 3)
bcoef(fm)

}

\keyword{datagen}