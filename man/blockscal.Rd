\name{blockscal}
\alias{blockscal}
\encoding{latin1}

\title{Block scaling}

\description{

Function \code{blockscal} scales the blocks (i.e. sets of columns) of a matrix, and concatenates the scaled blocks into a new matrix. Each block is scaled by its "norm": square root of the sum of the variances of each column of the block. The variances can be weighted  

The reference row observations can be weighted.

}

\usage{

blockscal(Xr, Xu = NULL, blocks, weights = rep(1, nrow(Xr)))

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block dispersion measure). Default to \code{NULL}.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{Xr} defining the given block. The same blocks are used for \eqn{Xu}.}

\item{weights}{A vector of length \eqn{n} defining the statistical weights to apply to the observations for the column variance calculation. }


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