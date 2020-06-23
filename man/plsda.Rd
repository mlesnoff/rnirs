\name{plsda}
\alias{plsda}
\alias{plsdalm}
\alias{pcda}
\encoding{latin1}

\title{PLSDA and PCDA}

\description{

- Function \code{plsda} fits PLSDA models, with various DA methods.

The class membership \eqn{y} (unidimensional variable) for the reference observations (= training) is firstly transformed to a table \eqn{Ydummy} containing \eqn{nclas} dummy variables (with function \code{\link{dummy}}), where \eqn{nclas} is the number of classes in \eqn{y}. Then, a PLS2 is implemented on the \eqn{X-}data and the dummy table \eqn{Ydummy} (with function \code{\link{pls}}), returning PLS2 scores finally used by the DA method.

The PLS2 algorithm is defined in argument \code{algo}.

The DA method is defined in argument \code{da}. The default DA method is \code{\link{dalm}}. Other methods presently avalaible are \code{\link{dadis}}, \code{\link{daglm}}, \code{\link{daprob}} and \code{\link{knnda}}. Any other ad'hoc DA methods can be developed by the user. Note that the DA method \code{\link{dasdod}} can not be used in \code{plsda}: in this last function, the number of PLS components (that corresponds to the input data for the DA) varies from 1 to \code{ncomp}; \code{\link{dasdod}} computes PCAs from the input data (here the PLS components) which cannot be implemented when the number of PLS comoonents is equal to 1 or 2.

- Function \code{plsdalm} is a faster equivalent of \code{plsda(..., da = dalm, ...)}.

- Function \code{pcda} does the same as \code{plsda} but on principal component (instead of PLS) scores. The syntax is the same.


}

\usage{

plsda(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, da = dalm, ...)

plsdalm(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel,  ...)

pcda(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pca.eigen, da = dalm, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (i.e. components) to consider.}

\item{algo}{A function (algorithm) implementing a PLS. Default to \code{\link{pls.kernel}} for PLSDA and \code{\link{pca.eigen}} for PCDA.}

\item{da}{A function defining the discriminant method used for the predictions. Default to \code{\link{dalm}}.} 

\item{...}{Optionnal arguments to pass in functions defined in \code{algo} and \code{da}.}

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

######## PLS-DALM

ncomp <- 20
fm <- plsda(Xr, yr, Xu, yu, ncomp = ncomp)
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

## Same with plsdalm (faster)

ncomp <- 20
fm <- plsdalm(Xr, yr, Xu, yu, ncomp = ncomp)
z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

######## PLS-LDA

ncomp <- 20
fm <- plsda(Xr, yr, Xu, yu, ncomp, da = daprob)

z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

######## PLS-QDA

ncomp <- 20
fm <- plsda(Xr, yr, Xu, yu, ncomp, da = daprob, 
  lda = FALSE)

z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

######## PLS-KNNDA

ncomp <- 15
fm <- plsda(Xr, yr, Xu, yu, ncomp, da = knnda, 
  diss = "mahalanobis", k = 3)
  
z <- err(fm, ~ ncomp + k)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = z$k)

}

\keyword{datagen}