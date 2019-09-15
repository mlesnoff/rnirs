\name{kenston}
\alias{kenston}
\encoding{latin1}

\title{Kennard-Stone sampling}

\description{

Sampling row observations of a data set with the Kennard-Stone algorithm (1969).

}

\usage{

kenston(X, m, diss = c("euclidean", "mahalanobis", "correlation"))

}

\arguments{

\item{X}{A matrix or data frame in which row observations are selected.}

\item{m}{An integer defining the number of observations to select.}

\item{diss}{The type of dissimilarity used for selecting the observations. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation" (correlation dissimilarities are calculated by sqrt(.5 * (1 - rho))).}

}

\value{

A vector of the indexes (i.e. row numbers) of the selected observations.

}

\references{

Kennard, R.W., Stone, L.A., 1969. Computer aided design of experiments. Technometrics, 11(1), 137-148.


}

\examples{

data(datcass)

X <- datcass$Xr

fm <- pca.svd(X, ncomp = 10)
s <- kenston(fm$T, m = 20, diss = "mahalanobis")
s

plot(fm$T[, 1:2]) 
abline(h = 0, v = 0, lty = 2)
points(fm$T[s, 1:2], col = "red", pch = 16)

}

\keyword{datagen}