\name{blocksopls}
\alias{blocksopls}
\alias{blocksopca}
\encoding{latin1}

\title{Block dimension reduction by SO-PLS or SO-PCA}

\description{

Functions \code{blocksopls} and \code{blocksopca} implements block dimension reduction, by PLS or PCA sequential orthogonalization (SO-PLS and SO-PCA, respectively), of a reference (= training) matrix and eventually a new (= test) matrix. 

SO-PLS is for instance described in Menichelli et al. (2014), Biancolillo et al. (2015) and Biancolillo (2016). SO-PCA uses the same approach but without needing response variables \eqn{Y}. 

The scores calculated for each block for the reference matrix and the eventual new matrix are concatenated into new output matrices (blocks of scores).

The reference row observations can be weighted.

}

\usage{

blocksopls(Xr, Yr, Xu = NULL, blocks, ncomp, ...)

blocksopca(Xr, Xu = NULL, blocks, ncomp, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block score spaces). Default to \code{NULL}.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{Xr} defining the given block. The same blocks are used for \eqn{Xu}.}

\item{ncomp}{A list of same length as the number of blocks defining the number of scores to calculate for each block, or a single number. In this last case, the same number of scores is used for all the blocks.}

\item{...}{Other arguments to pass in functions \code{\link{pls}} or \code{\link{pca}}.}

}

\value{

\item{Tr}{A matrix with the scores calculated from \eqn{Xr} and concatenated by block.}

\item{Tu}{A matrix with the scores calculated from \eqn{Xu} and then concatenated by block. \code{NULL} if \code{Xu = NULL}.}

\item{blocks}{A list of column numbers defining the blocks in the output matrices \eqn{Tr} and \eqn{Tu}.}

\item{Fitr and Fitu}{The SO-PLS \eqn{Y-}response predictions for the reference and new observations.}

}

\references{


- Biancolillo et al. , 2015. Combining SO-PLS and linear discriminant analysis for
multi-block classification. Chemometrics and Intelligent Laboratory Systems, 141, 58-67.

- Biancolillo, A. 2016. Method development in the area of multi-block analysis focused on food analysis. PhD. University of copenhagen.

- Menichelli et al., 2014. SO-PLS as an exploratory tool for path modelling. Food Quality and Preference, 36, 122-134.



}


\examples{

N <- 10 ; p <- 12
set.seed(1)
X <- matrix(rnorm(N * p, mean = 10), ncol = p, byrow = TRUE)
Y <- matrix(rnorm(N * 2, mean = 10), ncol = 2, byrow = TRUE)
colnames(X) <- paste("x", 1:ncol(X), sep = "")
colnames(Y) <- paste("y", 1:ncol(Y), sep = "")
set.seed(NULL)
X
Y

Xr <- X[1:8, ]
Yr <- Y[1:8, ]

Xu <- X[9:10, ]
Yu <- Y[9:10, ]

n <- nrow(Xr)
m <- nrow(Xu)


blocks <- list(1:4, 5:7, 9:ncol(X))
blocks

ncomp <- 2

fm <- blocksopls(Xr, Yr, Xu, blocks, ncomp)
fm

### With weights

w <- rep(1, n)
#w <- 1:n
fm <- blocksopls(Xr, Yr, Xu, blocks, ncomp, algo = pls.kernelw, weights = w)
fm

}

\keyword{datagen}