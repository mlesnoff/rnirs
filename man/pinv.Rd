\name{pinv}
\alias{pinv}
\encoding{latin1}

\title{Moore-Penrose pseudo-inverse of a matrix}

\description{

Calculation of the Moore-Penrose (MP) pseudo-inverse of a matrix \eqn{X}.

}

\usage{

pinv(X, tol = sqrt(.Machine$double.eps))

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

\item{tol}{A relative tolerance to detect zero singular values. }

}



\value{

A list of outputs, such as:

\item{Xplus}{The MP pseudo-inverse.}

}


\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- rnorm(n)
set.seed(NULL)

pinv(X)

tcrossprod(pinv(X)$Xplus, t(y))
lm(y ~ X - 1)

}

\keyword{datagen}