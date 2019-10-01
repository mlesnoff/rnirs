\name{plsda}
\alias{plsda}
\alias{plsdalm}
\encoding{latin1}

\title{PLSDA}

\description{

Function \code{plsda} fits PLSDA models, with various DA methods.

The class membership (unidimensional variable) \eqn{y} for the reference (= training) observations is firstly transformed (with function \code{\link{dummy}}) to a table \eqn{Ydummy} containing \eqn{nclas} dummy variables, where \eqn{nclas} is the number of classes in \eqn{y}. Then, a PLS2 is implemented on the \eqn{X-}data and the dummy table \eqn{Ydummy}, returning PLS2 scores finally used by the DA method.

The PLS2 algorithm is defined in argument \code{algo}.

The DA method is defined in argument \code{da}. The default DA method is \code{\link{dalm}}. Other methods presently avalaible are \code{\link{dadis}}, \code{\link{daglm}}, \code{\link{daprob}} and \code{\link{knnwda}}. Any other ad'hoc DA methods can be developed by the user. Note that the DA method \code{\link{dasdod}} can not be used in \code{plsda}: in this last function, the number of PLS components (that corresponds to the input data for the DA) varies from 1 to \code{ncomp}; \code{\link{dasdod}} computes PCAs from the input data (here the PLS components) which cannot be implemented when the number of PLS comoonents is equal to 1 or 2.

Function \code{plsdalm} is a faster equivalent of \code{plsda(..., da = dalm, ...)}.

}

\usage{

plsda(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, da = dalm, ...)

plsdalm(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (i.e. components) to consider.}

\item{algo}{A function (algorithm) implementing a PLS. Default to \code{\link{pls.kernel}}.}

\item{da}{A function defining the discriminant method used for the predictions. Default to \code{\link{dalm}}.} 

\item{...}{Optionnal arguments to pass in functions defined in \code{algo} and \code{da}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

And other outputs: See \code{\link{pls}} and the function defined in \code{algo}.


}

\examples{

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

table(yr)
table(yu)


## PLS-DALM

ncomp <- 20
fm <- plsda(Xr, yr, Xu, yu, ncomp = ncomp, da = dalm)
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- err(fm, ~ ncomp)
plotmse(z, nam = "errp")
z[z$errp == min(z$errp), ]

### Same with plsdalm

ncomp <- 20
fm <- plsdalm(Xr, yr, Xu, yu, ncomp = ncomp)
z <- err(fm, ~ ncomp)
plotmse(z, nam = "errp")
z[z$errp == min(z$errp), ]

## PLS-QDA

ncomp <- 20
fm <- plsda(Xr, yr, Xu, yu, ncomp, da = daprob)
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- err(fm, ~ ncomp)
z
plotmse(z, nam = "errp")
z <- z[z$errp == min(z$errp), ]
z

# Same as:
# (for ncomp = z$ncomp components)
u <- pls(Xr, dummy(yr), Xu, ncomp = z$ncomp)
fm <- daprob(u$Tr, yr, u$Tu, yu)
err(fm)


## PLS-LDA

fm <- plsda(Xr, yr, Xu, yu, ncomp, da = daprob, lda = TRUE)
z <- err(fm, ~ ncomp)
plotmse(z, nam = "errp")
z[z$errp == min(z$errp), ]


## PLS-KNNDA

fm <- plsda(Xr, yr, Xu, yu, ncomp, da = knnwda, diss = "mahalanobis", k = 3)
z <- err(fm, ~ ncomp + k)
plotmse(z, nam = "errp", group = "k")
z[z$errp == min(z$errp), ]


}

\keyword{datagen}