\name{kpcr}
\alias{kpcr}
\encoding{latin1}

\title{Non linear kernel PCR Models}

\description{

Regression on kernel PCA latent variables (scores). The functions are implemented only for algorithms providing orthogonal scores.

The kernel Gram matrix is internally centered before the analyses, but the data are not column-wise scaled. If needed, the user has to do the scaling before using the function  (there is no argument \code{scale} in the function). 

Row observations can eventually be weighted (using argument \code{weights}).

}

\usage{

kpcr(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, weights = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of scores (= components = latent variables) to consider.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

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
fm <- kpcr(Xr, Yr, Xu, Yu, ncomp = ncomp, degree = 3)
names(fm)
z <- mse(fm, ~ ncomp)
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

## fictive weights
kpcr(Xr, Yr, Xu, Yu, ncomp = ncomp, weights = 1:nrow(Xr))

}

\keyword{datagen}