\name{matpoly}
\alias{matpoly}
\encoding{latin1}

\title{Blocks for INLR}

\description{

Function \code{matpoly} builds blocks useable for "implicit non-linear latent variable regression (INLR)" (Berglund & Wold 1997). 

Given an input data matrix \eqn{X = [ x_ij ]} and a degree \eqn{d}, the function builts a matrix resulting from the concatenation of the \eqn{d} blocks \eqn{[ (x_ij)^k ] ; k = 1, ..., d} (corresponding to the successive polynoms without the cross-product terms). 

}

\usage{
matpoly(X, degree = 2, blockscal = FALSE)
}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{degree}{The degree used for the concatenation.}

\item{blockscal}{If \code{TRUE}, the blocks are internally scaled by function \code{\link{blockscal}}. Default to \code{FALSE} .}

}


\references{

Berglund, A., Wold, S., 1997. INLR, implicit non-linear latent variable regression. Journal of Chemometrics 11, 141-156. https://doi.org/10.1002/(SICI)1099-128X(199703)11:2<141::AID-CEM461>3.0.CO;2-2

}

\examples{

n <- 5
p <- 3
m <- 3
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
Xu <- matrix(rnorm(m * p, mean = 10), ncol = p)
y <- rnorm(nrow(X))
set.seed(NULL)

matpoly(X, degree = 2)

zXr <- matpoly(X, degree = 3)$X
zXu <- matpoly(Xu, degree = 3)$X
fm <- plsr(zXr, y, zXu, ncomp = 2)
fm


}

\keyword{datagen}