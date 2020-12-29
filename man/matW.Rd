\name{matW}
\alias{matW}
\alias{matB}
\encoding{latin1}

\title{Between and within covariance matrices}

\description{
Calculation of within (\code{matW}) and between (\code{matB}) covariance matrices for classes of row observations of a data set.
}

\usage{

matW(X, y)

matB(X, y)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

\item{y}{A vector of length \eqn{n} defining the class membership of the observations.} 

}

\details{The denominator in the variance calculations is \eqn{n}.}

\value{

A matrix.

}


\examples{

n <- 8
p <- 3
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10, sd = 3), ncol = p, byrow = TRUE)
y <- sample(1:2, size = n, replace = TRUE)
set.seed(NULL)
X
y

matW(X, y)

matB(X, y)
  
matB(X, y)$B

matW(X, y)$W + matB(X, y)$B
(n - 1) / n * cov(X)

}

\keyword{datagen}