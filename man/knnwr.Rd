\name{knnwr}
\alias{knnwr}
\alias{knnwda}
\encoding{latin1}

\title{kNN-WR/WDA}

\description{

Functions \code{knnwr} and \code{knnwda} build kNN (eventually locally weighted) regression models for a quantitative (\code{knnlwr}) or qualitative (\code{knnlwda}) univariate response \eqn{y}.

The functions use functions \code{\link{getknn}} and \code{\link{locw}}. See the code for details

For each new observation to predict, the principle of kNN regression models (R and DA) is to select a number of \eqn{k} nearest neighbors and to calculate the prediction by the average of the response \eqn{y} (for regression) or the most frequent class in \eqn{y} (for discrimination) over this neighborhood. The kNN selection step is referred to as \eqn{weighting 1} in \code{\link{locw}}. In standard kNN regression models, the statistical weight of each of the \eqn{k} neighbors is \eqn{1/k}. In locally weighted kNN regression models, the statistical weights of the neighbors depend from the dissimilarities (preliminary calculated) between the observation to predict and the \eqn{k} neighbors. This step is referred to as \eqn{weighting 2} in \code{\link{locw}}.

In \code{knnwr} and \code{knnwda}, the dissimilarities can be calculated from the original (i.e. not compressed) data or from preliminary computed global PLS scores.  

}

\usage{

knnwr(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  stor = TRUE,
  print = TRUE
  ) 

knnwda(
  Xr, Yr,
  Xu, Yu = NULL,
  ncompdis = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  h = Inf, k,
  stor = TRUE,
  print = TRUE
  )

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses(quantitative variable or class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (quantitative variable or class membership). Default to \code{NULL}.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "euclidean" (default; Euclidean distance), "mahalanobis" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{ncompdis}{TA vector (eventually of length = 1) defining the number(s) of components of the preliminary global PLSR calculated on \eqn{(Xr, Yr)} and \eqn{Xu} for calculating the  dissimilarities used for defining the neighbors. If \code{NULL} (default; no preliminary data compression), the dissimilarities are calculated from the original data \eqn{Xr} and \eqn{Xu}.Each component of \code{ncompdis} is considered successively in the calculations.}

\item{h}{A vector (eventually of length = 1) defining the scaling shape factor(s) of the function of the weights applied to the neighbors in the weighted PLSR. Lower is \eqn{h}, sharper is the function. See \code{\link{wkern}}.Each component of \code{h} is considered successively in the calculations.}

\item{k}{A vector (eventually of length = 1) defining the number(s) of nearest neighbors to select in the reference data set for each observation to predict. Each component of \code{k} is considered successively in the calculations.}

\item{stor}{See \code{\link{locw}}.}

\item{print}{Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{
Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
}

\examples{

library(ggplot2)

data(datcass)
data(datforages)

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

ncompdis <- 10
h <- c(1, 2)
k <- seq(5, 20, by = 5)
fm <- knnwr(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- mse(fm, ~ ncompdis + h + k)
z
z[z$rmsep == min(z$rmsep), ]

u <- z
u$group <- paste(u$ncompdis, u$h)
p <- ggplot(data = u, aes(x = k, y = rmsep))
p <- p + geom_point(aes(colour = as.factor(group)), size = 2)
p


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

ncompdis <- 10
h <- c(1, 2)
k <- seq(5, 15, by = 5)
fm <- knnwda(
  Xr, yr,
  Xu, yu,
  ncompdis = ncompdis, diss = "mahalanobis",
  h = h, k = k,
  print = TRUE
  )
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
z <- err(fm, ~ ncompdis + h + k)
z
z[z$err == min(z$errp), ]

u <- z
u$group <- paste(u$ncompdis, u$h)
p <- ggplot(data = u, aes(x = k, y = errp))
p <- p + geom_point(aes(colour = as.factor(group)), size = 2)
p

}

\keyword{datagen}