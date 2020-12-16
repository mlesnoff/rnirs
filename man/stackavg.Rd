\name{stackavg}
\alias{stackavg}
\alias{stackavgcla}
\encoding{latin1}

\title{Stacking for predictions models}

\description{

Model stacking (here = averaging) for regression (quantitative response) and discrimination (qualitative response) models. 

Functions \code{stackavg} and  \code{stackavgcla} calculate "averages", eventually with \eqn{a priori} weighting, of univariate predictions returned by a set of different models:

- Arithmetic mean of the quantitative predictions for regression models;

- Vote of the predicted classes for discrimination models. 

A weighting of the predictions in the mean and vote is allowed (argument \code{weights}).

}

\usage{

stackavg(fit, y = NULL, formula, nam = NULL, weights = NULL)

stackavgcla(fit, y = NULL, formula, nam = NULL, weights = NULL)

}

\arguments{

\item{fit}{Data frame of predicted values.}

\item{y}{Data frame of \eqn{y-}data. If the \eqn{y-}data do not exist, set to \code{NULL} (default).}

\item{formula}{A right-hand-side formula defing the aggregation levels on which the predictions are averaged.}

\item{nam}{Name (character string) of the column to consider in \code{fit} and \code{y} for calculating the averaging. If \code{NULL} (default), the last column of \code{fit} and \code{y} are considered.}

\item{weights}{A vector (of length the number of rows in \code{y} and \code{fit}) of eventual weights to apply for the averaging or vote. If \code{NULL} (default), this is the standard mean or vote (all weights are set to 1).}

}


\examples{

############## Regression

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr
Xu <- datcass$Xu
yu <- datcass$yu

ncomp <- 25
fm <- plsr(Xr, yr, Xu, yu, ncomp = ncomp)

z <- mse(fm, ~ ncomp)
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

w <- ifelse(fm$y$ncomp >= 5 & fm$y$ncomp <= 20, 1, 0)
zfm <- stackavg(fm$fit, fm$y, formula = ~ rownum + rownam, weights = w)
names(zfm)
head(zfm$y)
head(zfm$fit)
head(zfm$r)

mse(zfm)


############## Discrimination

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr
Xu <- datforages$Xu
yu <- datforages$yu

ncomp <- 20
fm <- plsdalm(Xr, yr, Xu, yu, ncomp = ncomp)
z <- err(fm, ~ ncomp)
z[z$errp == min(z$errp), ]
plotmse(z, nam = "errp")

w <- ifelse(fm$y$ncomp >= 8 & fm$y$ncomp <= 20, 1, 0)
zfm <- stackavgcla(fm$fit, fm$y, formula = ~ rownum + rownam, weights = w)
names(zfm)
head(zfm$y)
head(zfm$fit)
head(zfm$r)

err(zfm)

}

\keyword{datagen}