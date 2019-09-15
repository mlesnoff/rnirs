\name{matdis}
\alias{matdis}
\encoding{latin1}

\title{Dissimilarity matrix (between observations)}

\description{
Calculation of a dissimilarity matrix for a data set: dissimilarities calculated between each of the row observations of the data set.
}

\usage{matdis(X, diss = c("euclidean", "mahalanobis", "correlation"))}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame. }

\item{diss}{Type of dissimilarities calculated. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).} 

}

\value{A \eqn{n x n} distance matrix.}

\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
X

matdis(X)
i <- 2 ; j <- 3
sqrt(sum((X[i, ] - X[j, ])^2))

matdis(X, diss = "mahalanobis")
i <- 2
sqrt(mahalanobis(X, center = X[i, ], cov = cov(X)))

}

\keyword{datagen}