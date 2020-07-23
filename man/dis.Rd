\name{dis}
\alias{dis}
\encoding{latin1}

\title{Dissimilarities between row observations of a matrix and a given vector}

\description{

Calculation of dissimilarities between the row observations of a data set and a given vector \eqn{mu}. By default, if not specified, vector \eqn{mu} is the column means of the reference (= training) data set.

}

\usage{

dis(Xr, Xu = NULL, mu = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  sigma = NULL, weights = NULL)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations (\eqn{Xu} is not used in the calculation of the median and MAD used for calculating the standardized dissimilarities; see Details).}

\item{mu}{The vector of length \eqn{p} which is compared to the \eqn{n} rows of \eqn{Xr} and, if not \code{NULL}, to the \eqn{m} rows \eqn{Xu}. If \code{mu} is \code{NULL} (default), \code{mu} is set to the column means of \eqn{Xr}. }

\item{diss}{The type of dissimilarities used. Possible values are "euclidean" (Euclidean distances; default), "mahalanobis" (Mahalanobis distances), or "correlation". Correlation dissimilarities are calculated by \eqn{sqrt(.5 * (1 - rho))}.}

\item{sigma}{For Mahalanobis distance, the covariance matrix considered. If \code{NULL} (default), \code{sigma} is set to \code{cov(Xr)}.}

\item{weights}{Only for Mahalanobis distance and when \code{sigma = NULL}. A vector of length \eqn{n} defining a priori weights to apply to the training observations, for the computation of the covariance matrix. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

A list of outputs, such as:

\item{dr}{A data frame of the dissimilarities for Xr.}

\item{du}{A data frame of the dissimilarities for Xu.}

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

Xr <- X[1:6, ] ; Yr <- Y[1:6, ]
Xu <- X[7:8, ] ; Yu <- Y[7:8, ]

mu <- NULL
dis(Xr, mu = mu)

dis(Xr, Xu, mu)

fm <- pls(Xr, Yr, ncomp = 3)
dis(fm$Tr, mu = mu, diss = "mahalanobis")

fm <- pls(Xr, Yr, Xu, ncomp = 3)
dis(fm$Tr, fm$Tu, mu, diss = "mahalanobis")

}

\keyword{datagen}