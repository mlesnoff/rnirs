\name{plsr}
\alias{plsr}
\alias{pcr}
\alias{bcoef}
\encoding{latin1}

\title{PCR and PLSR models}

\description{

\code{pcr} fits PCR models.

\code{plsr} fits PLSR (1 or 2) models.

Function \code{bcoef} returns the "b-coefficients" (= regression coefficients for the \eqn{X}-matrix), including the intercept, from a PCR or PLSR model.

}

\usage{

pcr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pca.svd, ...)

plsr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, ...)

bcoef(fm, ncomp = NULL)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (i.e. components) to consider. For \code{bcoef}, the default is \code{NULL} (the \code{ncomp} scores computed in the PLSR are considered).}

\item{algo}{A function (algorithm) implementing a PLS. Default to \code{\link{pls.kernel}}.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

\item{fm}{For \code{bcoef}, output of \code{plsr}, \code{\link{pls}} or the function defined in \code{algo}.}

}

\value{

For \code{plsr}, a list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

And other outputs: See \code{\link{pls}} and the function defined in  \code{algo}.

}


\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp)
#fm <- plsr(Xr, Yr, Xu, ncomp = ncomp)
#fm <- plsr(Xr, Yr[, 1], Xu, Yu[, 1], ncomp = ncomp)
#fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, algo = pls.kernelw, weights = rep(1, nrow(Xr)))
######## PCR
#fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp)
#fm <- pcr(Xr, Yr, Xu, Yu, ncomp = ncomp, algo = pca.svdw, weights = rep(1, nrow(Xr)))
names(fm)
fm$y
fm$fit
fm$r
mse(fm, ~ ncomp, nam = "y1")
mse(fm, ~ ncomp, nam = "y2")
mse(fm, ~ ncomp)  ## y2

z <- mse(fm, ~ ncomp)
z
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

bcoef(fm)
bcoef(fm, ncomp = 2)

fm$fit[fm$fit$ncomp == 2, ]
cbind(rep(1, nrow(Xu)), Xu) %*% bcoef(fm, ncomp = 2)

}

\keyword{datagen}