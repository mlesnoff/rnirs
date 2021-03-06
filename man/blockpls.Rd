\name{blockpls}
\alias{blockpls}
\encoding{latin1}

\title{Block dimension reduction by PCA or PLS}

\description{

Function \code{blockpls} implements a dimension reduction of pre-selected blocks of variables (= set of columns) of a reference (= training) matrix and eventually a new (= test) matrix, by PCA (default) or PLS. The block reduction consists in calculating latent variables (= scores) independently for each block. 

The scores calculated for each block for the reference matrix are concatenated into a new output matrix. The same is done for the eventual test scores.

The function allows giving a priori weights to the rows of the reference matrix in the calculations (argument \code{weights} to specify within the optional arguments "\code{...}").

}

\usage{

blockpls(Xr, Yr = NULL, Xu = NULL, blocks, colblocks = NULL, ncomp, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. If \code{NULL} (default), the reduction methods is PCA; in the other case, it is PLS.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block score spaces). Default to \code{NULL}.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{Xr} defining the given block. The same blocks are used for \eqn{Xu}.}

\item{colblocks}{Alternative to using argument \code{blocks}. A numeric vector of length \eqn{p} giving the index of the block for each of the columns of \eqn{Xr} (the same vector is used for \eqn{Xu}). Default to \code{NULL} (argument \code{blocks} is used instead).}

\item{ncomp}{A list of same length as the number of blocks defining the number of scores to calculate for each block, or a single number. In this last case, the same number of scores is used for all the blocks.}

\item{...}{Other arguments to pass in functions \code{\link{pls}} or \code{\link{pca}}.}

}

\value{

\item{Tr}{A matrix with the scores calculated from \eqn{Xr} and concatenated by block.}

\item{Tu}{A matrix with the scores calculated from \eqn{Xu} and then concatenated by block. \code{NULL} if \code{Xu = NULL}.}

\item{blocks}{A list of column numbers defining the blocks in the output matrices \eqn{Tr} and \eqn{Tu}.}

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

res <- blockpls(X[1:3, ], Xu = X[4:5, ], blocks = blocks, ncomp = 2) 
#res <- blockpls(X[1:3, ], Xu = X[4:5, ], colblocks = c(rep(1, 3), rep(2, 3), rep(3, 3)), ncomp = 2) 
#res <- blockpls(X[1:3, ], Y[1:3, ], X[4:5, ], blocks = blocks, ncomp = c(2, 1, 1)) 
res

}

\keyword{datagen}