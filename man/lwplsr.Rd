\name{lwplsr}
\alias{lwplsr}
\alias{lwplsda}
\alias{lwplsdalm}

\encoding{latin1}

\title{kNN-LWPLSR/DA}

\description{

Wrappers for k-nearest-neighbors locally weighted PLSR and PLSDA (kNN-LWPLSR/DA) models: 

- Function \code{lwplsr} builds kNN-LWPLSR models 

- Functions \code{lwplsda} and \code{lwplsdalm} build kNN-LWPLSDA models. 

These wrappers use functions \code{\link{getknn}}, \code{\link{locw}} and functions for PLSR (\code{\link{plsr}}) or PLSDA (\code{\link{plsda}} and \code{\link{plsdalm}}). See the code for details.

Function \code{lwplsdalm} is a faster equivalent of \code{lwplsda(..., da = dalm, ...)}.

LWPLSR is a "local"  modelling approach (a different model is built for each new observation to predict) and a particular case of weighted PLSR (WPLSR) . In WPLSR, specific statistical weights, different from the usual \eqn{1/n} of the standard PLSR, and changing for each new observation to predict, are given to the \eqn{n} training observations. These weights are used in the calculation of (a) the PLS scores and loadings and (b) the regression of the response over the scores. The particularity of LWPLSR wihin the WPLSR models is that the weights depend from dissimilarities (e.g. distances) between this new observation to predict and the training observations.

The basic LWPLSR strategy uses, for each observation to predict, the \eqn{n} training observations. This can be very time consuming, in particular for large \eqn{n}. A faster and often more efficient strategy is to preliminary select in the training set a number of \eqn{k} nearest neighbors to the observation to predict and then to apply LWPLSR only to this pre-selected neighborhood. This corresponds to kNN-LWPLSR. 

In function \code{locw} (see the help page), the preliminary kNN selection step is referred to as \eqn{weighting 1}, and the weighting of the \eqn{k} neighbors to as \eqn{weighting 2}. 

kNN-LWPLSDA uses the same principle, though WPLSDA is used in place of WPLSR.

In functions \code{lwplsr}, \code{lwplsda} and \code{lwplsdalm}, the dissimilarities used for computing the weights can be calculated from the original (i.e. without dimension reduction) \eqn{X-}data or, when using argument \code{ncompdis}, from preliminary computed global PLS scores.  

}

\usage{

lwplsr(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  ncomp,
  stor = TRUE,
  print = TRUE
  )

lwplsda(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  ncomp,
  stor = TRUE,
  print = TRUE,
  ...
  )

lwplsdalm(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  ncomp,
  stor = TRUE,
  print = TRUE
  )

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{For quantitative responses: A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. For qualitative responses: A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{For quantitative responses: A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. For qualitative responses: A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{ncompdis}{A vector (eventually of length = 1) defining the number(s) of components of the preliminary global PLSR calculated on \eqn{(Xr, Yr)} and \eqn{Xu} for calculating the  dissimilarities used for defining the neighbors. If \code{NULL} (default), there is no preliminary data compression), i.e. the dissimilarities are calculated from the original \eqn{Xr} and \eqn{Xu} data.Each component of \code{ncompdis} is considered successively in the calculations.}

\item{h}{A vector (eventually of length = 1) defining the scaling shape factor(s) of the function of the weights applied to the neighbors in the weighted PLSR. Lower is \eqn{h}, sharper is the function. See \code{\link{wkern}}.Each component of \code{h} is considered successively in the calculations.}

\item{k}{A vector (eventually of length = 1) defining the number(s) of nearest neighbors to select in the reference data set for each observation to predict. Each component of \code{k} is considered successively in the calculations.}

\item{ncomp}{The maximum number(s) of components considered in the local PLSR models. The predictions are returned for models having from 1 to \eqn{ncomp} components.}

\item{stor}{See \code{\link{locw}}.}

\item{print}{Logical. If \code{TRUE} (default), fitting information are printed.}

\item{...}{Only for \code{lwplsda}. Other arguments to pass in \code{plsda} and in the function set in its argument \code{da}.}

}

\references{

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.M.. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR Data. Submitted to Journal of Chemometrics.

Schaal, S., Atkeson, C., Vijayamakumar, S. 2002. Scalable techniques from nonparametric statistics for the real time robot learning. Applied Intell., 17, 49-60.

Sicard, E. Sabatier, R., 2006. Theoretical framework for local PLS1 regression and application to a rainfall data set. Comput. Stat. Data Anal., 51, 1393-1410.

}

\examples{


\dontrun{

data(datcass)
data(datforages)

################################ lwplsr

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

### A kNN-LWPLSR model where:
### The dissimilarities between the observations are defined
### by the Mahalanobis distances calculated in a global PLS score space
### of ncompdis = 10 components.
### - Weighting 1 = selection of k nearest neighbors
### - Weighting 2 = weights within each neighborhood calculated with "wkern" 

k <- c(50, 100)
ncompdis <- 10
h <- c(1, 2)
ncomp <- 20
fm <- lwplsr(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- mse(fm, ~ ncompdis + h + k + ncomp)
head(z)
z[z$rmsep == min(z$rmsep), ]

u <- z
u$group <- paste(u$ncompdis, u$h, u$k)
plotmse(u, group = "group")

### An approach for decreasing the calculation time
### (and in some cases increasing the results stability 
### and decreasing the error rates) is to replace matrices Xr and Xu 
### by global PLSR score matrices Tr and Tu

zfm <- pls(Xr, yr, Xu, ncomp = 25) # calculation of the new data
zXr <- zfm$Tr
zXu <- zfm$Tu

k <- c(50, 100)
ncompdis <- 10
h <- c(1, 2)
ncomp <- 20
fm <- lwplsr(
  zXr, yr,
  zXu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  ncomp = ncomp,
  print = TRUE
  )
z <- mse(fm, ~ ncompdis + h + k + ncomp)
head(z)
z[z$rmsep == min(z$rmsep), ]

u <- z
u$group <- paste(u$ncompdis, u$h, u$k)
plotmse(u, group = "group")

######################################### lwplsda

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

### A kNN-LWPLSDA model (with dalm) where:
## The dissimilarities between the observations are defined
## by the Mahalanobis distances calculated from a global PLS score space
## of ncompdis = 10 components.
## - Weighting 1 = knn selection of k = {5, 10, 15} neighbors
## - Weighting 2 = within each neighborhood, weights are calculated by "wkern" 

ncompdis <- 10
h <- 2
k <- c(50, 100)
ncomp <- 15
fm <- lwplsda(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  da = dalm,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = "k")

### Same using lwplsdalm (faster)

ncompdis <- 10
h <- 2
k <- c(50, 100)
ncomp <- 15
fm <- lwplsdalm(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  ncomp = ncomp,
  print = TRUE
  )
z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = "k")

### Same models but changing the DA method ==> LDA

ncompdis <- 10
h <- 2
k <- c(50, 100)
ncomp <- 15
fm <- lwplsda(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  da = daprob, lda = TRUE,
  ncomp = ncomp,
  print = TRUE
  )
z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = "k")

}

}


