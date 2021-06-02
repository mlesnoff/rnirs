\name{orthog}
\alias{orthog}
\encoding{latin1}

\title{Orthogonalization of a matrix to another matrix}

\description{

Function \code{orthog} orthogonalizes a matrix \eqn{Y} to a matrix \eqn{X}. The row observations can be weighted. 

The function uses function \code{\link{lm}}.

}

\usage{

orthog(X, Y, weights = NULL)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

\item{Y}{A \eqn{n x q} matrix or data frame to orthogonalize to \eqn{X}.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

\item{Y}{The \eqn{Y} matrix orthogonalized to \eqn{X}.}

\item{b}{The regression coefficients used for orthogonalization.}

}

\examples{

n <- 8 ; p <- 3 ; q <- 2
set.seed(1)
X <- matrix(rnorm(n * p), ncol = p)
Y <- matrix(rnorm(n * q), ncol = q)
colnames(Y) <- c("y1", "y2")
set.seed(NULL)
X
Y

res <- orthog(X, Y)
res$Y
crossprod(res$Y, X)     # ===> orthogonal
res$b

# Same as:
fm <- lm(Y ~ X - 1)
Y - fm$fitted.values
fm$coef

#### WITH WEIGHTS

w <- 1:n
fm <- lm(Y ~ X - 1, weights = w)
Y - fm$fitted.values
fm$coef

res <- orthog(X, Y, weights = w)
res$Y
crossprod(res$Y, w * X)       # ===> orthogonal in metric w
res$b

}

\keyword{datagen}