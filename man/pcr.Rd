\name{pcr}
\alias{pcr}
\encoding{latin1}

\title{PCR models}

\description{

Fitting PCR1 or PCR2 models.

}

\usage{

pcr(Xr, Yr, Xu, Yu = NULL, ncomp, meth = pca, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (= components = latent variables) to consider.}

\item{meth}{A function of PCA (default to \code{\link{pca}}).}

\item{...}{Optionnal arguments to pass in the function defined in \code{meth}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{fm}{Output of the function defined in \code{meth}.}

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
fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp)
names(fm)
fm$y
fm$fit
fm$r
names(fm$fm)

pcr(Xr, Yr, Xu, ncomp = ncomp)[c("y", "fit", "r")]

pcr(Xr, Yr[, 1], Xu, Yu[, 1], ncomp = ncomp)[c("y", "fit", "r")]

####### B-COEFFICIENTS FOR THE MODEL WITH ncomp COMPONENTS

b <- bcoef(fm)
b

cbind(rep(1, nrow(Xu)), Xu) %*% b
fm$fit[fm$fit$ncomp == ncomp, ]

####### MSE

ncomp <- 3
fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp)
mse(fm, ~ ncomp, nam = "y1")
mse(fm, ~ ncomp, nam = "y2")
mse(fm, ~ ncomp)

z <- mse(fm, ~ ncomp)
z
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

####### PCR WITH A NON-LINEAR KERNEL PCA
### (Note: b coefficients are not availbale in that case).

fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp, meth = kpca, degree = 3)



}

\keyword{datagen}