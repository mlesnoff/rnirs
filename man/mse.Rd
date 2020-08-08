\name{mse}
\alias{mse}
\alias{err}
\alias{plotmse}
\encoding{latin1}

\title{Prediction error rates}

\description{

Calculation and plotting of prediction error rates (RMSEP, SEP, etc. or classification error rate)  for models with quantitative or qualitative responses. 

Note: In function \code{mse}, The \eqn{R2} rate is calculated by \eqn{R2 = 1 - MSEP(fitted model) / MSEP(no model)}. For predictions over CV or Test sets, and/or for non linear models, it can be different from the square of the correlation coefficient between the observed values and prediction. 

See examples in \code{\link{plsr}}, \code{\link{plsda}}, etc.

}

\usage{

mse(fm, formula = ~ 1, nam = NULL, digits = 3)

err(fm, formula = ~ 1, nam = NULL, digits = 4)

plotmse(obj, nam = "rmsep", namx = "ncomp", group = NULL,  col = NULL,
  legend = TRUE, legend.title = NULL, ncol = 1, ...)

}

\arguments{

\item{fm}{An object containing at least data frames \code{y} (data) and \code{fit} (predictions). See for instance objects \code{y} and \code{fit} returned by function \code{\link{plsr}}.}

\item{formula}{A right-hand-side formula defing the aggregation levels on which are calculated the error rates.}

\item{nam}{For \code{mse}: Name (character string) of the column to consider in \code{fm$y} for calulating the prediction error rate. If \code{NULL} (default), the last column of \code{fm$y} is considered. For \code{plotmse}: Name (character string) of the column in \code{obj} to plot as y-axis. Default to \code{nam = "rmsep"}.}

\item{namx}{For \code{plotmse}: Name (character string) of the column in \code{obj} to plot as x-axis. Default to \code{nam = "ncomp"}.}

\item{digits}{The number of digits for the numerical outputs.}

\bold{Arguments specific to \code{plotmse}:}

\item{obj}{Data frame returned by \code{\link{mse}} or \code{\link{err}}.}

\item{group}{A vector of length equal to the number of rows of \code{obj} defining the groups of rows to be plotted with different colors (default is \code{NULL}).}

\item{col}{A color, or a vector of colors (of length equal to the number of groups), defining the color(s) of the lines representing the error rates.}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{...}{Other arguments to pass in functions \code{\link{plot}} or \code{\link{lines}}}.

}

\keyword{datagen}