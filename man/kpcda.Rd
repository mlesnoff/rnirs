\name{kpcda}
\alias{kpcda}
\alias{kpcdalm}
\encoding{latin1}

\title{KPCDA}

\description{

Discrimination (DA) on non linear kernel PCA latent variables.

Functions \code{kpcda} and \code{kpcdalm} do the same as \code{pcda} and \code{pcdalm} (see correponding help pages) but on scores returned by a kernel PCA, instead of usual PCA.

Function \code{kpcdalm} is a faster equivalent of \code{kpcda(..., da = dalm, ...)}.

The kernel Gram matrix is internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the function). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted (using argument \code{weights}).

}

\usage{

kpcda(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, da = dalm, ...)

kpcdalm(Xr, Yr, Xu, Yu = NULL, ncomp, kern = kpol, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp}{The number of scores (i.e. components) to consider.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{da}{A function defining the discriminant method used for the predictions. Default to \code{\link{dalm}}.} 

\item{...}{Optionnal arguments to pass in functions defined in \code{kern} and \code{da}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\examples{

data(datforages)
Xr <- datforages$Xr
yr <- datforages$yr
Xu <- datforages$Xu
yu <- datforages$yu
Xr <- detrend(Xr)
Xu <- detrend(Xu)
headm(Xr)
headm(Xu)
table(yr)
table(yu)

######## KPC-DALM

ncomp <- 20
fm <- kpcda(Xr, yr, Xu, yu, ncomp = ncomp, kpol = krbf, sigma  = 2)
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

## Same with kpcdalm (faster)

ncomp <- 20
fm <- kpcdalm(Xr, yr, Xu, yu, ncomp = ncomp, kpol = krbf, sigma  = 2)
z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

######## KPC-QDA

ncomp <- 20
fm <- kpcda(Xr, yr, Xu, yu, ncomp, da = daprob, degree = 3, lda = FALSE)
z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

}

\keyword{datagen}