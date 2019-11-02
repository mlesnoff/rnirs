\name{blockscal}
\alias{blockscal}
\encoding{latin1}

\title{Block scaling}

\description{

Function \code{blockscal} scales blocks (i.e. sets of columns) of a matrix by a block-dispersion measure, and concatenates the scaled blocks into a new matrix. 

The function uses function \code{\link{blocksel}}.

The default block-dispersion measure is the sum of the standard deviations of the columns of the block.

}

\usage{

blockscal(Xr,Xu = NULL, blocks, stat = sd)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block dispersion measure). Default to \code{NULL}.}

\item{blocks}{A list of same size as the number of blocks. Each component of the list has to give the column numbers in \eqn{Xr} defining the given block. The same blocks are use for \eqn{Xu} if not \code{NULL}.}

\item{stat}{A function giving the dispersion measure. Default to \code{\link{sd}}.}


}

\value{

\item{Xr}{A matrix with the concatenated scaled blocks from \eqn{Xr}.}

\item{Xu}{A matrix with the concatenated scaled blocks from \eqn{Xu}. \code{NULL} if \code{Xu = NULL}.}

\item{blocks}{A list of column numbers defining the blocks in \eqn{Xr}, and eventually in \eqn{Xu}.}

}

\examples{

n <- 5 ; p <- 10
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
colnames(X) <- paste("Var", 1:p, sep = "")
set.seed(NULL)
X

blocks <- list(1:2, 4, 6:8)
blocks
res <- blockscal(X[1:3, ], X[4:5, ], blocks)
res

blockscal(res$Xr, blocks = res$blocks)

}

\keyword{datagen}