\name{mse}
\alias{mse}
\alias{err}
\alias{plotmse}
\encoding{latin1}

\title{Prediction error rates}

\description{

Calculation and plotting of prediction error rates for models with quantitative (MSEP, RMSEP, etc.) or qualitative (classification error rate) response. 

Note: In function \code{mse}, The \eqn{R2} rate is calculated by \eqn{R2 = 1 - MSEP(fitted model) / MSEP(no model)}. For predictions over CV or Test sets, and/or for non linear models, it can be different from the square of the correlation coefficient between the observed values and prediction. 

See examples in \code{\link{plsr}}, \code{\link{plsda}}, etc.

}

\usage{

mse(fm, formula = ~ 1, nam = NULL, digits = 3)

err(fm, formula = ~ 1, nam = NULL, digits = 4)

plotmse(obj, nam = "rmsep", group = NULL, scale.x.cont = TRUE)

}

\arguments{

\item{fm}{An object containing at least data frames \code{y} (data) and \code{fit} (predictions). See for instance objects \code{y} and \code{fit} returned by function \code{\link{plsr}}.}

\item{formula}{A right-hand-side formula defing the aggregation levels on which is calculated the error rate.}

\item{nam}{For \code{mse}: Name (character string) of the column to consider in \code{fm$y} for calulating the prediction error rate. If \code{NULL} (default), the last column of \code{fm$y} is considered. For \code{plotmse}: Name (character string) of the column to plot in \code{obj}. Default is \code{nam = "rmsep"}.}

\item{digits}{The number of digits for the numerical outputs.}

\bold{Arguments specific to \code{plotmse}:}

\item{obj}{Data frame returned by \code{\link{mse}} or \code{\link{err}}.}

\item{group}{Name (character string) of column of \code{obj} defining the groups of observations to be plotted with different colors (default is \code{NULL}).}

\item{scale.x.cont}{Logical. If \code{TRUE} (default), the xlab-scale is numbered with a step of 1.}

}

\keyword{datagen}