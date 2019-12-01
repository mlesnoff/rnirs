\name{dadis}
\alias{dadis}
\encoding{latin1}

\title{DA using the dissimilarity to class centers}

\description{

Discriminant analysis using the dissimilarities (e.g. Euclidean or Mahalanobis distances) to the class centers. 

For each observation to predict, the function calculates the dissimilarity between this observation and each of the class centers of the reference (= training) data. The final predicted class corresponds to the class for which the dissimilarity is the lowest.

}

\usage{
dadis(Xr, Yr, Xu, Yu = NULL,
  diss = c("euclidean", "mahalanobis", "correlation"), sigma = NULL)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{diss}{Type of dissimilarity used. Possible values are "euclidean" (Euclidean distances; default), "mahalanobis" (Mahalanobis distances), or "correlation" (correlation dissimilarities, calculated by \eqn{sqrt(.5 * (1 - rho))} where rho is the correlation coefficient).}

\item{sigma}{The covariance matrix (weight matrix) used for the Mahalanobis distance. If \code{NULL} (default), this is the average 'Within' covariance matrix (\eqn{W}) calculated by function \code{\link{matW}} for each class.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}


\examples{

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
N <- nrow(X)

m <- round(.25 * N) 
n <- N - m          
s <- sample(1:N, m)
Xr <- X[-s, ]
yr <- y[-s]
Xu <- X[s, ]
yu <- y[s]

fm <- dadis(Xr, yr, Xu, yu)
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)
fm$ni

err(fm)

fm <- dadis(Xr, yr, Xu, yu, diss = "mahalanobis")
err(fm)

nclas <- length(unique(yr))
W <- matW(Xr, yr)$W * n / (n - nclas)
fm <- dadis(Xr, yr, Xu, yu, diss = "mahalanobis", sigma = W)
err(fm)

}

\keyword{datagen}