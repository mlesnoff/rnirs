\name{plsr}
\alias{plsr}
\alias{pcr}
\encoding{latin1}

\title{PCR and PLSR Models}

\description{

Regression on PCA or PLS latent variables (scores). 

Functions \code{pcr} and \code{plsr} assume PCA and PLS algorithms providing orthogonal scores.

Matrix \eqn{X} is centered before the analyses, but \eqn{X} is not column-wise scaled (there is no argument \code{scale} available). If a scaling is needed, the user has to scale \eqn{X} before using the functions. 

Row observations can eventually be weighted (using argument \code{weights}).

}

\usage{

pcr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = NULL, ...)

plsr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The maximal number of PLS scores (= components = latent variables) to consider.}

\item{algo}{For \code{plsr}, a function (algorithm) implementing a PLS on the training data set. Default to  \code{NULL} (\code{\link{pls_kernel}} is used). For \code{pcr}, a function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp)
#fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp)
names(fm)
fm$y
fm$fit
fm$r
names(fm)

plsr(Xr, Yr, Xu, ncomp = ncomp)[c("y", "fit", "r")]

plsr(Xr, Yr[, 1], Xu, Yu[, 1], ncomp = ncomp)[c("y", "fit", "r")]

####### MSE

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp)
mse(fm, ~ ncomp, nam = "y1")
mse(fm, ~ ncomp, nam = "y2")
mse(fm, ~ ncomp)

z <- mse(fm, ~ ncomp)
z
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

####### B-COEFFICIENTS FOR THE MODEL WITH ncomp COMPONENTS

bcoef(fm, ncomp = 2)

####### Weighted PLSR

# fictive weights
plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, weights = 1:nrow(Xr))

}

\keyword{datagen}