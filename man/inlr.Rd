\name{inlr}
\alias{inlr}
\encoding{latin1}

\title{Blocks for INLR}

\description{

Function \code{inlr} builds matrix blocks useable for "implicit non-linear latent variable regression (INLR)" (Berglund & Wold 1997). INLR is in the same idea of direct non linear kernel regression and discrimination methods (see \code{\link{kgram}}).

Given an input reference (=training) data matrix \eqn{X = [ x_ij ]} and a degree \eqn{d > 1}, the function builds a new matrix resulting from the concatenation of the \eqn{d} blocks \eqn{[ (x_ij)^k ] ; k = 1, ..., d} corresponding to the successive polynoms (without the cross-product terms).

The same is done for an eventual matrix of new data to predict (= test matrix). If there is a block scaling for the training matrix (see argument \code{scale.blocks}), the same scaling values are used for the test matrix.

}

\usage{
inlr(Xr, Xu = NULL, degree = 2, scale.blocks = TRUE)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{degree}{The degree used for the concatenation. Must be \eqn{> 1}.}

\item{scale.blocks}{If \code{TRUE} (default), the blocks are internally scaled by function \code{\link{blockscal}}.}

}

\value{

A list of outputs, see the examples.

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

inlr(X, degree = 2)
inlr(X, X[1:2, ], degree = 2)

res <- inlr(X, degree = 3, scale.blocks = TRUE)
res
blockscal(res$Xr, colblocks = res$colblocks)$xdisptot

res <- inlr(X, Xu, degree = 3)
fm <- plsr(res$Xr, y, res$Xu, ncomp = 2)
fm


}

\keyword{datagen}