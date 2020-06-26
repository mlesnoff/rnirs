\name{plsr}
\alias{plsr}
\encoding{latin1}

\title{PLSR models}

\description{

Regression on PLS latent variables (scores).

Data are internally centered before the analyses, but they are not column-wise scaled. If needed, the user has to do the scaling before using the function (there is no argument \code{scale} in the function).  

}

\usage{

plsr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (= components = latent variables) to consider.}

\item{algo}{A function (algorithm) implementing a PLS on the training data set. Default to \code{\link{pls.kernel}}.}

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
names(fm)
fm$y
fm$fit
fm$r
names(fm$fm)

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