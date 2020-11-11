\name{sampks}
\alias{sampks}
\encoding{latin1}

\title{Kennard-Stone sampling}

\description{

Sampling row observations of data \eqn{X} with the Kennard-Stone (KS) algorithm (Kennard & Stone, 1969). 

The KS algorithm separates the data in two sets. These two sets do not have the same probability distribution. One set has higher variability than the other. Function \code{sampks} returns the set with the higher variability. Note that this may be the opposite in other softwares. 

}

\usage{

sampks(X, m, diss = c("euclidean", "mahalanobis", "correlation"))

}

\arguments{

\item{X}{A matrix or data frame in which row observations are selected.}

\item{m}{An integer defining the number of observations to select.}

\item{diss}{The type of dissimilarity used for selecting the observations in the KS algorithm. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

}

\value{

A vector of the indexes (i.e. row numbers in \eqn{X}) of the selected observations.

}

\references{

Kennard, R.W., Stone, L.A., 1969. Computer aided design of experiments. Technometrics, 11(1), 137-148.


}

\examples{

data(datcass)

X <- datcass$Xr

fm <- pca.eigenk(X, ncomp = 10)
s <- sampks(fm$T, m = 20, diss = "mahalanobis")
s

plotxy(fm$T, zeroes = TRUE, pch = 16) 
points(fm$T[s, 1:2], col = "red", pch = 16)

}

\keyword{datagen}