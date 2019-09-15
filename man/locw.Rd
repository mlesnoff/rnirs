\name{locw}
\alias{locw}

\encoding{latin1}

\title{Locally weighted models}

\description{

\code{locw} is a generic function for building kNN locally weighted (LW) prediction models. See the help page of function \code{\link{lwplsr}} for wrappers. 

In kNN-LW models, the prediction is implemented in two sequential steps, therafter referred to as \eqn{weighting 1} and \eqn{weighting 2}, respectively. For each new observation to predict, the two steps are as follow:

- \eqn{Weighting 1} corresponds to a "binary" weighting. The \eqn{k} nearest neighbors (in the training data set) of the obervation to predict are selected and constitute the neighborhood. The prediction model (implemented in the next step) is only run on this neighborhood. It equivalent to give a weight = 1 to all the observation in the neighborhood, and a weight = 0 to the other training observations.

- \eqn{Weighting 2} is a within-neighborhood weighting. Each of the \eqn{k} nearest neighbors receives a statistical weight (eventually different from the usual \eqn{1/k} as in the standard PLS) that is entered as input in the prediction model. The can weights depend from dissimilarities (preliminary calculated) between the new observation to predict and the \eqn{k} neighbors. 

The overall principle of \code{locw} is as follows. The prediction model used in step \eqn{2} has to be defined in a separate function specified in argument \code{fun}. If there is a number of \eqn{m} new observations to predict, a list of \eqn{m} vectors (defining the \eqn{m} neighborhoods) has to be provided as input to \code{locw} in argument \code{listnn}. Each of the \eqn{m} vectors contains the indexes of the nearest neighbors (in the training set) of the observation to predict. The \eqn{m} vectors are not necessary of same length, i.e. the neighborhood size can vary between observations to predict.  Then  \code{locw} runs the prediction model successively over the \eqn{m} neighborhoods, returning \eqn{m} predictions.    

}

\usage{

locw(
  Xr = NULL, Yr,
  Xu = NULL, Yu = NULL,
  listnn,
  listw = NULL,
  fun,
  stor = TRUE,
  print = TRUE,
  ...
  )
  
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{For quantive responses: A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. For qualitative responses: A vector of length \eqn{n} of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{For quantive responses: A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. For qualitative responses: A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response. Default to \code{NULL}.}

\item{listnn}{A list of \eqn{m} vectors defining weighting 1. Component \eqn{i} of this list is a vector (of length between 1 and \eqn{n}) of the indexes of the reference observations to consider as nearest neighbors for the new observation \eqn{i} to predict. Typically, \code{listnn} can be built from \code{\link{getknn}}, but any other list of length \eqn{m} can be provided. The \eqn{m} vectors have equal length (i.e. the \eqn{m} neighborhood are of equal size (i.e. the \eqn{m} observations to predict have the same number of neighbors) or not (the number of neighbors varies between the observations to predict).}

\item{listw}{A list of \eqn{m} vectors defining weighting 2. Component \eqn{i} of this list is a vector (must have the same length as component \eqn{i} of \code{listnn}) of the statistical weights of the nearest neighbors, used in the prediction model.}

\item{fun}{A function defining the prediction model to run on the \eqn{m} neighborhoods. The output of the function defined in \code{fun} must be a list with at least the three components \code{y}, \code{fit} and \code{r} (see for instance the outputs of \code{\link{plsr}}).}

\item{stor}{Logical (default = \code{TRUE}). If \code{TRUE}, the training data and the prediction model used for each of the \eqn{m} observation to predict are saved in object \code{fm}. This can be memory consumming when large neighborhoods are defined.}

\item{print}{Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in function \code{fun}.}

}

\references{

Lesnoff, M., Metz, M., Roger, J.M.. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR Data. Submitted to Journal of Chemometrics.

}


\examples{

\dontrun{

data(datcass)
data(datforages)

#############------------- QUANTITATIVE RESPONSE

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

### A locally weighted PLSR model where:
### The dissimilarity between the observations are defined by the Mahalanobis distance 
### calculated from a global PLS score space of ncompdis = 10 components.
### - Weighting 1 = selection of k = 50 nearest neighbors
### - Weighting 2 = weights within each neighborhood calculated with "wkern" 

ncompdis <- 10
h <- 2
k <- 50
ncomp <- 20
z <- pls(Xr, yr, Xu, ncomp = ncompdis)
resn <- getknn(z$Tr, z$Tu, k = k, diss = "mahalanobis")
listnn <- resn$listnn
listw <- lapply(resn$listd, wkern, h = h)
fm <- locw(
  Xr, yr,
  Xu, yu,
  listnn = listnn,
  listw = listw,
  fun = plsr,
  algo = pls.kernelw,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- mse(fm, ~ ncomp + k)
z[z$rmsep == min(z$rmsep), ]
plotmse(z, group = "k")

### Without weighting 2

k <- 50
ncompdis <- 10
ncomp <- 20
z <- pls(Xr, yr, Xu, ncomp = ncompdis)
resn <- getknn(z$Tr, z$Tu, k = k, diss = "mahalanobis")
listnn <- resn$listnn
fm <- locw(
  Xr, yr,
  Xu, yu,
  listnn = listnn,
  fun = plsr,
  ncomp = ncomp,
  print = TRUE
  )
z <- mse(fm, ~ ncomp + k)
z[z$rmsep == min(z$rmsep), ]
plotmse(z, group = "k")

#############------------- QUALITATIVE RESPONSE

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

### A locally weighted PLS-LDA model where:
### The dissimilarity between the observations are defined by the Mahalanobis distance 
### calculated from a global PLS score space of ncompdis = 10 components.
### - Weighting 1 = selection of k = 50 nearest neighbors
### - Weighting 2 = weights within each neighborhood calculated with "wkern" 

k <- 50
ncompdis <- 10
h <- 2
ncomp <- 15
z <- pls(Xr, dummy(yr), Xu, ncomp = ncompdis)
resn <- getknn(z$Tr, z$Tu, k = k, diss = "mahalanobis")
listnn <- resn$listnn
listw <- lapply(resn$listd, wkern, h = h)
fm <- locw(
  Xr, yr,
  Xu, yu,
  listnn = listnn,
  listw = listw,
  fun = plsda,
  da = daprob,
  lda = TRUE,
  algo = pls.kernelw,
  ncomp = ncomp,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- err(fm, ~ ncomp + k)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = "k")

### A locally weighted PLSDA (non parametric) model on preliminary calculated global scores

z <- pls(Xr, dummy(yr), Xu, ncomp = 25)
zXr <- z$Tr
zXu <- z$Tu

k <- 100
ncompdis <- 10
h <- 2
ncomp <- 10
resn <- getknn(zXr[, 1:ncompdis], zXu[, 1:ncompdis], k = k, diss = "mahalanobis")
listnn <- resn$listnn
listw <- lapply(resn$listd, wkern, h = h)
fm <- locw(
  zXr, yr,
  zXu, yu,
  listnn = listnn,
  listw = NULL,
  fun = plsda, dens = dkern.gauss,
  da = daprob,
  ncomp = ncomp,
  print = TRUE
  )
z <- err(fm, ~ ncomp + k)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp", group = "k")

}

}

\keyword{datagen}