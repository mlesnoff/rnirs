\name{lwplsr}
\alias{lwplsr}
\alias{lwplsda}
\alias{lwplsdalm}

\encoding{latin1}

\title{kNN-LWPLSR & DA}

\description{

Pipelines (wrappers) for k-nearest-neighbors locally weighted PLSR and PLSDA models (kNN-LWPLSR and kNN-LWPLSDA).

LWPLSR is a "local"  method: i.e. a different model is built for each new observation to predict, from a training set defined by neighboring. This is a particular case of weighted PLSR (WPLSR) (e.g. Schaal et al. 2002). In WPLSR, specific a priori weights, different from the usual \eqn{1/n} of the standard PLSR, are given to all the \eqn{n} training observations. These weights are used in the calculation of (a) the PLS scores and loadings and (b) the regression of the response over the scores. The specificity of LWPLSR is that the weights depend from dissimilarities (e.g. distances) between this new observation to predict and the training observations. Note that the weights change for each new observation to predict.

In the literature (Sicard & Sabatier 2006, Kim et al 2011), basic versions of LWPLSR use all the \eqn{n} training observations for each observation to predict. This can be very time consuming, in particular for large \eqn{n}. A faster and often more efficient strategy is to preliminary select in the training set a number of \eqn{k} nearest neighbors to the observation to predict (\eqn{weighting "1"} in function \code{\link{locw}}) and then to apply LWPLSR only to this pre-selected neighborhood (\eqn{weighting "2"} in \code{\link{locw}}). This corresponds to kNN-LWPLSR (Lesnoff et al. 2020). 

kNN-LWPLSDA uses the same principle, though WPLSDA is used in place of WPLSR.

- Function \code{lwplsr} builds kNN-LWPLSR models 

- Function \code{lwplsda} builds kNN-LWPLSDA models with various DA methods.

- Function \code{lwplsdalm} is a faster equivalent of \code{lwplsda(..., da = dalm, ...)}.

These pipelines use functions \code{\link{getknn}}, \code{\link{locw}} and functions for PLSR and PLSDA (code{\link{plsr}}, \code{\link{plsda}} and \code{\link{plsdalm}}). See the code for details. Many other pipelines can be build using function \code{\link{locw}}.

In functions \code{lwplsr}, \code{lwplsda} and \code{lwplsdalm}, the dissimilarities used for computing the weights can be calculated from the original \eqn{X-}data (i.e. without preliminary dimension reduction) or, when using argument \code{ncompdis}, from preliminary computed global PLS scores.  

Data are internally centered before the analyses, but not scaled (there is no argument \code{scale} in the functions). If needed, the scaling has to be done by the user before using the functions.  

}

\usage{

lwplsr(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = 5, k,
  ncomp,
  stor = TRUE,
  print = TRUE,
  ...
  )

lwplsda(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = 5, k,
  ncomp,
  stor = TRUE,
  print = TRUE,
  ...
  )

lwplsdalm(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = 5, k,
  ncomp,
  stor = TRUE,
  print = TRUE,
  ...
  )

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{For quantitative responses: A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. For qualitative responses: A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{For quantitative responses: A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. For qualitative responses: A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{ncompdis}{A vector (eventually of length = 1) defining the number(s) of components of the preliminary global PLS calculated on \eqn{(Xr, Yr)} and \eqn{Xu} for calculating the  dissimilarities used for defining the neighbors. If \code{NULL} (default), there is no preliminary data compression), i.e. the dissimilarities are calculated from the original \eqn{Xr} and \eqn{Xu} data.Each component of \code{ncompdis} is considered successively in the calculations.}

\item{h}{A scalar or vector of scalars defining the scaling shape factor(s) of the function of the weights applied to the neighbors in the weighted PLSR. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}. Each component of \code{h} is considered successively in the calculations. Default to \eqn{h = 5} which corresponds to a medium "omnibus" sharpness for the weight function (the user may decide to not optimze this parameter and use the default) .}

\item{k}{An integer of vector of integers defining the number(s) of nearest neighbors to select in the reference data set for each observation to predict. Each component of \code{k} is considered successively in the calculations.}

\item{ncomp}{The maximum number(s) of components considered in the local PLSR models. The predictions are returned for models having from 1 to \eqn{ncomp} components.}

\item{stor}{Logical (default to \code{TRUE}). See \code{\link{locw}}.}

\item{print}{Logical. If \code{TRUE} (default), fitting information are printed.}

\item{...}{Other arguments to pass in \code{plsr}, \code{plsda}, or the function set in its argument \code{da}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.-M., 2020. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR data. Journal of Chemometrics, e3209. https://doi.org/10.1002/cem.3209

Schaal, S., Atkeson, C., Vijayamakumar, S. 2002. Scalable techniques from nonparametric statistics for the real time robot learning. Applied Intell., 17, 49-60.

Sicard, E. Sabatier, R., 2006. Theoretical framework for local PLS1 regression and application to a rainfall data set. Comput. Stat. Data Anal., 51, 1393-1410.

}

\examples{

data(datcass)
data(datforages)

############################# lwplsr

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- detrend(Xr)
Xu <- detrend(Xu)
dim(Xr)
dim(Xu)

###### A kNN-LWPLSR model where:
## The dissimilarities between the observations are defined
## by the Mahalanobis distances calculated in a global PLS score space
## of ncompdis = 10 components.
## - Weighting 1 = selection of k nearest neighbors
## - Weighting 2 = weights within each neighborhood calculated with "wdist" 

ncompdis <- 10
h <- c(2, Inf)
k <- c(100, Inf)
ncomp <- 15
fm <- lwplsr(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- mse(fm, ~ ncompdis + h + k + ncomp)
headm(z)
z[z$rmsep == min(z$rmsep), ]

u <- z
group <- paste("h=", u$h, ", k=", u$k, sep = "")
plotmse(u, group = group)

###### An approach for decreasing the calculation time
## (and in some cases increasing the results stability 
## and decreasing the error rates) is to replace matrices Xr and Xu 
## by global PLSR score matrices Tr and Tu

zfm <- pls(Xr, yr, Xu, ncomp = 25) # calculation of the new data

ncompdis <- 10
h <- c(2, Inf)
k <- c(100, Inf)
ncomp <- 15
fm <- lwplsr(
  zfm$Tr, yr,
  zfm$Tu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  ncomp = ncomp,
  print = TRUE
  )

z <- mse(fm, ~ ncompdis + h + k + ncomp)
headm(z)
z[z$rmsep == min(z$rmsep), ]

u <- z
group <- paste("h=", u$h, ", k=", u$k, sep = "")
plotmse(u, group = group)


############################# lwplsda

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)

headm(Xr)
headm(Xu)

table(yr)
table(yu)

###### A kNN-LWPLSDA model (with dalm) where:
## The dissimilarities between the observations are defined
## by the Mahalanobis distances calculated from a global PLS score space
## of ncompdis = 10 components.
## - Weighting 1 = knn selection of k = {5, 10, 15} neighbors
## - Weighting 2 = within each neighborhood, weights are calculated by "wdist" 

ncompdis <- 10
k <- c(50, 100)
ncomp <- 15
fm <- lwplsda(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  k = k,
  da = dalm,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
group <- paste("h=", z$h, ", k=", z$k, sep = "")
plotmse(z, nam = "errp", group = group)

###### Same using lwplsdalm (faster)

ncompdis <- 10
k <- c(50, 100)
ncomp <- 15
fm <- lwplsdalm(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  k = k,
  ncomp = ncomp,
  print = TRUE
  )

z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
group <- paste("h=", z$h, ", k=", z$k, sep = "")
plotmse(z, nam = "errp", group = group)

## Same models but changing the DA method ==> LDA

ncompdis <- 10
k <- c(50, 100)
ncomp <- 15
fm <- lwplsda(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  k = k,
  da = daprob,
  ncomp = ncomp,
  print = TRUE
  )

z <- err(fm, ~ ncompdis + h + k + ncomp)
z[z$errp == min(z$errp), ]
group <- paste("h=", z$h, ", k=", z$k, sep = "")
plotmse(z, nam = "errp", group = group)

############################# OBJECTS RETURNED BY THE FUNCTIONS

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
row.names(X) <- paste("AA", 1:n, sep = "")
y1 <- 100 * rnorm(nrow(X))
y2 <- 100 * rnorm(nrow(X))
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X
Yr <- Y
Xu <- X[c(1, 2, 4), ] ; Yu <- Y[c(1, 2, 4), ]

fm <- lwplsr(
  Xr, Yr,
  Xu, Yu,
  ncompdis = 3, diss = "mahalanobis",
  k = 5,
  ncomp = 2,
  print = TRUE
  )
names(fm)
fm[c("y", "fit", "r")]

## fm$fm = A list whose each component contains the model outputs 
## for "one observation to predict x a parameter combination {ncompdis, h, k}"
names(fm$fm)

## Sub-model i
i <- 1
#i <- 2
#i <- 3
names(fm$fm[[i]])
fm$fm[[i]]
bcoef(fm$fm[[i]])

lscordis(fm)
lodis(fm, Xr, Xu)


}

