\name{kgram}
\alias{kgram}
\encoding{latin1}

\title{Kernel Gram matrices}

\description{

Function \code{kgram} builds kernel Gram matrices for reference (= training) observations and, eventually, new (= test) observations to predict. 

The generated Gram matrices can then be used as input of usual regression functions (e.g. \code{\link{plsr}} or \code{\link{plsda}}) for implementing "direct" kernel regressions or discriminations. For instance, the direct kernel PLSR (DKPLSR) is discussed in Bennet & Embrechts (2003). See the examples below.

}

\usage{

kgram(Xr, Xu = NULL, kern = kpol, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax, and other available kernel functions. The user can build any other ad'hoc kernel function.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}.}

}

\value{

\item{Kr}{The \eqn{n x n} training Gram matrix.}

\item{Ku}{The \eqn{m x n} test Gram matrix.}

}

\references{

Bennett, K.P., Embrechts, M.J., 2003. An optimization perspective on kernel partial least squares regression, in: Advances in Learning Theory: Methods, Models and Applications, NATO Science Series III: Computer & Systems Sciences. IOS Press Amsterdam, pp. 227-250.

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

}

\examples{

####### Example of fitting the function sinc(x) (Rosipal & Trejo 2001 p. 105-106) 

x <- seq(-10, 10, by = .2)
x[x == 0] <- 1e-5
n <- length(x)
zy <- sin(abs(x)) / abs(x)
y <- zy + rnorm(n, 0, .2)
plot(x, y, type = "p")
lines(x, zy, lty = 2)
Xu <- Xr <- matrix(x, ncol = 1)

## DKPLSR

res <- kgram(Xr, Xu, kern = krbf, sigma = 1)
ncomp <- 3
fm <- plsr(res$Kr, y, res$Ku, ncomp = ncomp)
fit <- fm$fit$y1[fm$fit$ncomp == ncomp]
plot(Xr, y, type = "p")
lines(Xr, zy, lty = 2)
lines(Xu, fit, col = "red")

## DK Ridge regression

res <- kgram(Xr, Xu, kern = krbf, sigma = 1)
fm <- lmridge(res$Kr, y, res$Ku, lambda = .1)
fit <- fm$fit$y1
plot(Xr, y, type = "p")
lines(Xr, zy, lty = 2)
lines(Xu, fit, col = "red")

}

\keyword{datagen}