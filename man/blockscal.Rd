\name{blockscal}
\alias{blockscal}
\encoding{latin1}

\title{Block scaling}

\description{

Function \code{blockscal} scales the blocks (i.e. sets of columns) of a reference (= training) matrix and eventually a new (= test) matrix, and concatenates the scaled blocks into two new output matrices. 

Each block of the reference matrix is scaled by its "norm", i.e. the square root of the sum of the variances of each column of the block. The blocks of the test matrix are scaled by the norms of the blocks of the reference matrix. 

These output matrices can be used in LMR, PLSR or PLSDA (or any other models) for \eqn{Y-}predictions, see examples in \code{\link{blocksopls}}.

The function allows giving particular weights to the rows of the reference matrix in the calculations (argument \code{weights} to specify within the optional arguments "\code{...}").

}

\usage{

blockscal(Xr, Xu = NULL, blocks, weights = NULL)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block dispersion measure). Default to \code{NULL}.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{Xr} defining the given block. The same blocks are used for \eqn{Xu}.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the training observations for the column variance calculation. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

\item{Xr}{A matrix with the concatenated scaled blocks from \eqn{Xr}.}

\item{Xu}{A matrix with the concatenated scaled blocks from \eqn{Xu}. \code{NULL} if \code{Xu = NULL}.}

\item{blocks}{A list of column numbers defining the blocks in the output matrices \eqn{Xr} and \eqn{Xu}.}

}

\examples{

n <- 5 ; p <- 10
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
colnames(X) <- paste("v", 1:p, sep = "")
set.seed(NULL)
X

blocks <- list(1:2, 4, 6:8)
blocks

res <- blockscal(X[1:3, ], X[4:5, ], blocks)
res

blockscal(res$Xr, blocks = res$blocks)

}

\keyword{datagen}