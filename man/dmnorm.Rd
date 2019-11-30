\name{dmnorm}
\alias{dmnorm}
\alias{dkern.gauss}
\encoding{latin1}

\title{Probability density prediction}

\description{

Prediction of the probability density of observations. Function \code{dmnorm} assumes a multivariate gaussian distribution for the reference (= training) observations. Function \code{dkern.gauss} returns a non parametric estimate using a (multiplicative) multivariate gaussian kernel estimator. 

}

\usage{

dmnorm(Xr = NULL, Xu, mu = NULL, sigma = NULL, diag = FALSE)

dkern.gauss(Xr, Xu, H = NULL, hs = NULL, a = .5)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations. For \code{dmnorm}, \code{Xr} is not used if arguments \code{mu} and \code{sigma} are not \code{NULL}.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations for which the probability density has to be predicted.}

Specific arguments for \code{dmnorm}:

\item{mu}{A \eqn{p} vector representing the mean of the gaussian distribution. If \code{NULL} (default), this is \code{colMeans(Xr)}.}

\item{sigma}{The \eqn{p x p} covariance matrix of the gaussian distribution. If \code{NULL} (default), this is \code{cov(Xr)}.}

\item{diag}{Logical indicating if the estimated covariance matrix is forced to be diagonal (default to \code{FALSE}). Ignored if sigma is not \code{NULL}.}

Specific arguments for \code{dkern.gauss}:

\item{H}{The \eqn{p x p} bandwidth matrix for the kernel estimator. If \code{NULL} (default): if \code{hs} is \code{NULL}, see the code.}

\item{hs}{A scalar representing a same bandwidth for all the \eqn{p} dimensions (matrix \eqn{H} is made diagonal with the value \eqn{hs}). Ignored if \code{H} is not \code{NULL}.}

\item{a}{A scaling scalar used if \code{H} and \code{hs} are \code{NULL}. See the code.}

}

\examples{

data(iris)

Xr <- iris[, 1:4]
yr <- iris[, 5]

fm <- fda(Xr, yr)
Tr <- fm$Tr

m <- 50
x1 <- seq(min(Tr[, 1]), max(Tr[, 1]), length.out = m)
x2 <- seq(min(Tr[, 2]), max(Tr[, 2]), length.out = m)
Tu <- expand.grid(x1, x2)
head(Tu)

## Parametric

z <- dmnorm(Tr[yr == "setosa", ], Tu)$fit$fit
mfit1 <- matrix(z, nrow = m)
z <- dmnorm(Tr[yr == "versicolor", ], Tu)$fit$fit
mfit2 <- matrix(z, nrow = m)
z <- dmnorm(Tr[yr == "virginica", ], Tu)$fit$fit
mfit3 <- matrix(z, nrow = m)
par(mfrow = c(2, 2))
contour(x1, x2, mfit1)
abline(h = 0, v = 0, lty = 2)
contour(x1, x2, mfit2)
abline(h = 0, v = 0, lty = 2)
contour(x1, x2, mfit3)
abline(h = 0, v = 0, lty = 2)
par(mfrow = c(1, 1))

## Non-parametric

hs <- .5
z <- dkern.gauss(Tr[yr == "setosa", ], Tu, hs = hs)$fit$fit
mfit1 <- matrix(z, nrow = m)
z <- dkern.gauss(Tr[yr == "versicolor", ], Tu, hs = hs)$fit$fit
mfit2 <- matrix(z, nrow = m)
z <- dkern.gauss(Tr[yr == "virginica", ], Tu, hs = hs)$fit$fit
mfit3 <- matrix(z, nrow = m)
par(mfrow = c(2, 2))
contour(x1, x2, mfit1)
abline(h = 0, v = 0, lty = 2)
contour(x1, x2, mfit2)
abline(h = 0, v = 0, lty = 2)
contour(x1, x2, mfit3)
abline(h = 0, v = 0, lty = 2)
par(mfrow = c(1, 1))

}

\keyword{datagen}