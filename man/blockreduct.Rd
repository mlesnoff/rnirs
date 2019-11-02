\name{blockreduct}
\alias{blockreduct}
\encoding{latin1}

\title{Block dimension reduction}

\description{

Function \code{blockreduct} implements block reduction on a reference (= training) matrix and eventually a new (= test) matrix. The block reduction consists in calculating latent variables (scores) for each block, by PCA (default) or PLS. 

The scores are calculated for each block of the reference matrix and then concatenated into a new matrix (blocks of scores). The eventual new observations (test matrix) are projected by block on these reference space scores.

The function uses function \code{\link{blocksel}}.

}

\usage{

blockreduct(Xr, Yr = NULL, Xu = NULL, blocks, ncomp, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. If \code{NULL} (default), the reduction methods is PCA; in the other case, it is PLS.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block score spaces). Default to \code{NULL}.}

\item{blocks}{A list of same size as the number of blocks. Each component of the list has to give the column numbers in \eqn{Xr} defining the given block. The same blocks are use for \eqn{Xu} if not \code{NULL}.}

\item{ncomp}{A list of same size as the number of blocks defining the number of scores to calculate for each block, or a single number. In this last case, the same number of scores is used for all the blocks.}

\item{...}{Other arguments to pass in functions \code{\link{pca}} or \code{\link{pls}}.}

}

\value{

\item{Tr}{A matrix with the scores calculated from \eqn{Xr} and concatenated by block.}

\item{Tu}{A matrix with the scores calculated from \eqn{Xu} and then concatenated by block. \code{NULL} if \code{Xu = NULL}.}

\item{blocks}{A list of column numbers defining the blocks in \eqn{Tr}, and eventually in \eqn{Tu}.}

}

\examples{

n <- 5 ; p <- 10
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
colnames(X) <- paste("Var", 1:p, sep = "")
Y <- matrix(rnorm(n * 2, mean = 10), ncol = 2, byrow = TRUE)
set.seed(NULL)
X
Y

blocks <- list(1:3, 4:6, 7:9)
blocks
res <- blockreduct(X[1:3, ], Xu = X[4:5, ], blocks = blocks, ncomp = 2) 
#res <- blockreduct(X[1:3, ], Y[1:3, ], X[4:5, ], blocks = blocks, ncomp = 2) 
res

}

\keyword{datagen}