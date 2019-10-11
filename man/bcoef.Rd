\name{bcoef}
\alias{bcoef}
\encoding{latin1}

\title{b-cefficients for a PLS model}

\description{

Function \code{bcoef} returns the "b-coefficients" (= regression coefficients for the \eqn{X}-matrix), including the intercept, from a PLS model.

}

\usage{

bcoef(fm, ncomp = NULL)

}

\arguments{

\item{fm}{Output of one of the functions implementing the PCA or PLS.}

\item{ncomp}{The number of PLS components (i.e. scores) to consider. If \code{NULL} (default), the maximal number of components computed in the PLS is considered.}

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

fm <- pls.kernel(X, Y, ncomp = 3)
bcoef(fm)
bcoef(fm, ncomp = 2)

fm <- pls(X, Y, ncomp = 3)
bcoef(fm)
bcoef(fm, ncomp = 2)

}

\keyword{datagen}