\name{bcoef}
\alias{bcoef}
\encoding{latin1}

\title{b-coefficients for a PCR or PLSR model}

\description{

Function \code{bcoef} returns the "beta-coefficients" (= regression coefficients on the scores) and the "b-coefficients" (= regression coefficients for the \eqn{X}-matrix), including the intercept) from a PCR or PLSR model.

}

\usage{

bcoef(fm, Y = NULL, ncomp = NULL)

}

\arguments{

\item{fm}{Output  of functions \code{\link{pls}} or \code{\link{plsr}}, or of the PLS algorithm functions.}

\item{Y}{Required for PCR or model with non-orthogonal scores. The training \eqn{Y}-data used to compute the PCR or PLSR model.}

\item{ncomp}{The b-coefficients are calculated for the model with \code{ncomp} scores. If \code{NULL} (default), the maximal model is considered.}

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

fm <- plsr(X, Y, X, ncomp = 3)
#fm <- pls(X, Y, ncomp = 3)
#fm <- pls.kernel(X, Y, ncomp = 3)
bcoef(fm, ncomp = 2)

fm <- pcr(X, Y, X, ncomp = 3)
#fm <- pca(X, ncomp = 3)
#fm <- pca.eigen(X, ncomp = 3)
bcoef(fm, Y, ncomp = 2)





}

\keyword{datagen}