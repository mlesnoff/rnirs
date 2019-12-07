\name{blocksopls}
\alias{blocksopls}
\alias{blocksopca}
\encoding{latin1}

\title{Block dimension reduction by SO-PLS or SO-PCA}

\description{

Functions \code{blocksopca} and \code{blocksopls} implement dimension reduction of pre-selected blocks of variables (= set of columns) of a reference (= training) matrix and eventually a new (= test) matrix, by sequential orthogonalization-PCA or -PLS (SO-PCA and SO-PLS), respectively. 

SO-PLS is for instance described in Menichelli et al. (2014), Biancolillo et al. (2015) and Biancolillo (2016). SO-PCA uses the same approach but replacing PLS by PCA (in such case, response variables \eqn{Y} are not required). The block reduction consists in calculating latent variables (= scores) for each block, each block being sequentially orthogonalized to the information computed from the previous blocks.

The scores calculated for the different blocks of the reference matrix are concatenated into a new output matrix (= concatenation of the blocks of scores). The same is done for the eventual test matrix.

These output matrices can be used in LMR, PLSR or PLSDA (or any other models) for \eqn{Y-}predictions, see examples.

The function allows giving particular weights to the rows of the reference matrix in the calculations (argument \code{weights} to specify within the optional arguments "\code{...}").

}

\usage{

blocksopca(Xr, Xu = NULL, blocks, ncomp, ...)

blocksopls(Xr, Yr, Xu = NULL, blocks, ncomp, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations, which receives the same block-scaling as \eqn{Xr} (\eqn{Xu} is not used in the calculation of the block score spaces). Default to \code{NULL}.}

\item{blocks}{A list of same length as the number of blocks. Each component of the list gives the column numbers in \eqn{Xr} defining the given block. The same blocks are used for \eqn{Xu}.}

\item{ncomp}{A vector of same length as the number of blocks defining the number of scores to calculate for each block, or a single number. In this last case, the same number of scores is used for all the blocks.}

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

blocksopls(Xr, Yr, Xu, blocks, ncomp)

############# Syntax with weights

w <- rep(1, n)
#w <- 1:n
blocksopls(Xr, Yr, Xu, blocks, ncomp, 
  algo = pls.kernelw, weights = w)

############# Syntax for a SO-PLSR

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- detrend(Xr, method = "lowess")
Xu <- detrend(Xu, method = "lowess")

blocks <- list(1:500, 501:1050)
ncomp <- c(10, 5)
zfm <- blocksopls(Xr, yr, Xu, blocks = blocks, ncomp = ncomp)
fm <- lmr(zfm$Tr, yr, zfm$Tu, yu)
headm(fm$fit)
mse(fm)
plot(fm$fit$y1, fm$y$y1)
abline(0, 1)

zfm <- blocksopls(Xr, yr, Xu, blocks = blocks, ncomp = ncomp)
zncomp <- ncol(zfm$Tr)
fm <- plsr(zfm$Tr, yr, zfm$Tu, yu, ncomp = zncomp)
headm(fm$fit)
z <- mse(fm, ~ ncomp)
plotmse(z)
zncomp <- 7
z[z$ncomp == zncomp, ]
y <- fm$y
fit <- fm$fit
plot(fit$y1[fit$ncomp == zncomp], y$y1[y$ncomp == zncomp])
abline(0, 1)

############# Syntax for a SO-PLSDA

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

Xr <- detrend(Xr)
Xu <- detrend(Xu)

table(yr)
table(yu)

blocks <- list(1:350, 351:700)
ncomp <- 10
zfm <- blocksopls(Xr, dummy(yr), Xu, blocks = blocks, ncomp = ncomp)
fm <- daprob(zfm$Tr, yr, zfm$Tu, yu)
headm(fm$fit)
err(fm)

}

\keyword{datagen}