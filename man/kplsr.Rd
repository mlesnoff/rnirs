\name{kplsr}
\alias{kplsr}
\alias{kpcr}
\encoding{latin1}

\title{Non linear kernel PLSR and PCR Models}

\description{

Function \code{kplsr} fits a KPLSR model such as in Rosipal & Trejo (2001), with the NIPALS algorithm (\code{\link{kpls.nipals}}). 

Function \code{kpcr} fits a KPCR model, i.e. a regression on KPCA latent variables (scores) (see \code{\link{kpca}}).

The kernel Gram matrices are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function  . 

Row observations can eventually be weighted (using argument \code{weights}).

\bold{Note:} Alternative kernel PLSR or PCR algorithms, that can be much faster for large \eqn{n}, are to run "direct" kernel models, using function \code{\link{kgram}}.  


}

\usage{

kplsr(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, weights = NULL, ...)

kpcr(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, weights = NULL, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of scores (= components = latent variables) to consider.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax, and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

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
#fm <- kpcr(Xr, Yr, Xu, Yu, ncomp = ncomp, degree = 3)
names(fm)
z <- mse(fm, ~ ncomp)
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

## fictive weights
kplsr(Xr, Yr, Xu, Yu, ncomp = ncomp, weights = 1:nrow(Xr))

####### Example of fitting the function sinc(x) (Rosipal & Trejo 2001 p. 105-106) 

x <- seq(-10, 10, by = .2)
x[x == 0] <- 1e-5
n <- length(x)
zy <- sin(abs(x)) / abs(x)
y <- zy + rnorm(n, 0, .2)
plot(x, y, type = "p")
lines(x, zy, lty = 2)
Xu <- Xr <- .matrix(x, row = FALSE)

ncomp <- 3
fm <- kplsr(Xr, y, Xu, ncomp = ncomp, kern = krbf)
fit <- fm$fit$y1[fm$fit$ncomp == ncomp]
plot(Xr, y, type = "p")
lines(Xr, zy, lty = 2)
lines(Xu, fit, col = "red")

}

\keyword{datagen}