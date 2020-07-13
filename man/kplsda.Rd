\name{kplsda}
\alias{kplsdalm}
\alias{kpcda}
\alias{kpcdalm}
\encoding{latin1}

\title{Non linear kernel PLSDA and PCDA models}

\description{

Discrimination (DA) on non linear kernel PLS or PCA latent variables (scores).

Functions \code{kplsda} and \code{kplsdalm} do the same as \code{\link{plsda}} and \code{\link{plsdalm}}, respectively, but on scores returned by a kernel PLS (instead of the usual PLS scores). The kernel PLS scores are calculated with function \code{\link{kpls}}.

Functions \code{kpcda} and \code{kpcdalm} do the same as \code{\link{pcda}} and \code{\link{pcdalm}}, respectively, but on scores returned by a kernel PCA (instead of the usual PCA scores). The kernel PLS scores are calculated with function \code{\link{kpca}}.

The kernel Gram matrices are internally centered before the analyses, but the data are not column-wise scaled (there is no argument \code{scale} in the functions). If needed, the user has to do the scaling before using the function.

Row observations can eventually be weighted (using argument \code{weights}).

Alternative kernel algorithms, that can be much faster for large \eqn{n}, are to do "direct" kernel PLSDA or PCDA, using function \code{\link{kgram}}.  

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

######## KPCDALM

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

######## KPCQDA

ncomp <- 20
fm <- kpcda(Xr, yr, Xu, yu, ncomp, da = daprob, degree = 3, lda = FALSE)
z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

}

\keyword{datagen}