\name{stackavg}
\alias{stackavg}
\encoding{latin1}

\title{Stacking for regression models}

\description{

The function implements a model stacking (also referred to as model averaging or ensemblism) for regression models, i.e. an averaging (eventually weighted) of the predictions of a set of regression models . 


}

\usage{

stackavg(fit, y = NULL, formula, nam = NULL, weights = NULL)

}

\arguments{

\item{fit}{Data frame of predicted values.}

\item{y}{Data frame of \eqn{y-}data. If the \eqn{y-}data do not exist, set to \code{NULL} (default).}

\item{formula}{A right-hand-side formula defing the aggregation levels on which the predictions are averaged.}

\item{nam}{Name (character string) of the column to consider in \code{fit} and \code{y} for calculating the averaging. If \code{NULL} (default), the last column of \code{fit} and \code{y} are considered.}

\item{weights}{A vector (of length the number of rows in \code{y} and \code{fit}) of eventual weights to apply for the averaging. If \code{NULL} (default), this is the standard mean.}

}


\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr
Xu <- datcass$Xu
yu <- datcass$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)

ncomp <- 30
fm <- plsr(Xr, yr, Xu, yu, ncomp = ncomp)

z <- mse(fm, ~ ncomp)
plotmse(z)
z[z$rmsep == min(z$rmsep), ]

w <- ifelse(fm$y$ncomp >= 5 & fm$y$ncomp <= 20, 1, 0)
zfm <- stackavg(fm$fit, fm$y, formula = ~ rownum + rownam, weights = w)
names(zfm)
head(zfm$y)
head(zfm$fit)
head(zfm$r)

mse(zfm)

}

\keyword{datagen}