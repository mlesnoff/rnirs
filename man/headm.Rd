\name{headm}
\alias{headm}
\encoding{latin1}

\title{Return the first part of a matrix or data frame}

\description{

Function \code{headm} returns the first part and the dimension of a matrixor data frame.

}

\usage{

headm(X)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

}

\value{

See the example.

}



\examples{

n <- 1000
p <- 200
X <- matrix(rnorm(n * p), nrow = n)

headm(X)

}

\keyword{datagen}