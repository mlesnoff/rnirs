\name{sampdp}
\alias{sampdp}
\encoding{latin1}

\title{Duplex sampling}

\description{

The function divides the data \eqn{X} in two sets, "train" vs "test", using the Duplex algorithm (Snee, 1977). 

The training and test sets returned by the algorithm are of equal size. The evential remaining observations can be added \eqn{a posteriori} to the training set.

}

\usage{

sampdp(X, k, diss = c("euclidean", "mahalanobis", "correlation"))

}

\arguments{

\item{X}{A matrix or data frame in which row observations are selected.}

\item{k}{An integer defining the number of training observations to select. Must be <= \eqn{n / 2}.}

\item{diss}{The type of dissimilarity used for selecting the observations in the algorithm. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

}

\value{

A list of vectors of the indexes (i.e. row numbers in \eqn{X}) of the selected observations.

}

\references{

Kennard, R.W., Stone, L.A., 1969. Computer aided design of experiments. Technometrics, 11(1), 137-148.

Snee, R.D., 1977. Validation of Regression Models: Methods and Examples. Technometrics 19, 415-428. https://doi.org/10.1080/00401706.1977.10489581


}

\examples{

set.seed(seed = 1)
n <- 10 ; p <- 3
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(seed = NULL)
k <- 3

sampdp(X, k = 4)
sampdp(X, k = 4, diss = "mahalanobis")

###################################

data(datcass)

X <- datcass$Xr

fm <- pca_eigenk(X, ncomp = 10)
z <- sampdp(fm$T, k = 20, diss = "mahalanobis")
z

plotxy(fm$T, zeroes = TRUE, pch = 16) 
points(fm$T[z$test, 1:2], col = "red", pch = 16, cex = 1.3)

}

\keyword{datagen}