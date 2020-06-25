\name{kpol}
\alias{kpol}
\alias{krbf}
\alias{ktanh}
\encoding{latin1}

\title{Kernel}

\description{

Functions building the Gram matrix for different kernels (e.g. Scholkopf & Smola 2002).

}

\usage{

kpol(X, Y = NULL, degree = 1, scale = 1, offset = 0)

krbf(X, Y = NULL, sigma = 1)

ktanh(X, Y = NULL, scale = 1, offset = 0)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{Optional (default to \code{NULL}). A \eqn{m x p} matrix or data frame of variables.}

\item{degree}{See the code.}

\item{scale}{See the code.}

\item{offset}{See the code.}

\item{sigma}{See the code.}

}

\details{

If \code{Y = NULL}, the function returns the \eqn{n x n} Gram matrix \eqn{k(X, X)}. If not, the function returns the \eqn{m x n} Gram matrix \eqn{k(Y, X)}.

}

\references{

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

}

\examples{

n <- 5
p <- 4
m <- 2
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
Xu <- matrix(rnorm(m * p, mean = 10), ncol = p)
set.seed(NULL)

ncomp <- 3
kpol(X, degree = 2)
kpol(Xu, X, degree = 2)

}

\keyword{datagen}