\name{orthog}
\alias{orthog}
\encoding{latin1}

\title{Orthogonalization of a matrix to another matrix}

\description{

Function \code{orthog} orthogonalizes a matrix \eqn{Y} to a matrix \eqn{X}. The row observations can be weighted. 

The function uses function \code{\link{lm}}.

}

\usage{

orthog(X, Y, weights = rep(1, nrow(X)))

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame.}

\item{Y}{A \eqn{n x q} matrix or data frame to orthogonalize to \eqn{X}.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the row observations.}

}

\value{

\item{Y}{The \eqn{Y} matrix orthogonalized to \eqn{X}.}

\item{b}{The regression coefficients used for orthogonalization.}

}

\examples{

n <- 8 ; p <- 3
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
Y <- matrix(rnorm(n * 2, mean = 10), ncol = 2, byrow = TRUE)
colnames(Y) <- c("y1", "y2")
set.seed(NULL)
X
Y

fm <- lm(Y ~ X)
Y - fm$fitted.values
Yortho <- orthog(X, Y)
Yortho
t(Yortho) %*% X

w <- 1:nrow(X)
d <- w / sum(w)
fm <- lm(Y ~ X, weights = d)
Y - fm$fitted.values
Yortho <- orthog(X, Y, weights = d)
Yortho
t(Yortho) %*% diag(d) %*% X

}

\keyword{datagen}