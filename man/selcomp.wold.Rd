\name{selncomp.wold}
\alias{selncomp.wold}
\encoding{latin1}

\title{Heuristic selection of the number of components in a PCR/PLSR or PLSDA model with the Wold's criterion}

\description{

The function helps to select the number of components in a PCR/PLSR or PLSDA model using a heuristic criterion (Wold, 1978). The raw criterion is \eqn{R = 1 - r(a+1) / r(a)} where \eqn{r} is the error rate considered in the calculation (msep, rmsep, classification error rate, etc.) and \eqn{a} the model dimension (nb. components). \eqn{R} represents the relative gain in efficiency after a new dimension is added in the model. The iterations continue until \eqn{R} becomes lower than a threshold value \eqn{alpha} (by default, \eqn{alpha} is set to .01 in the function).

Two other variants are proposed (argument \code{typ}):

- An "integral" version in which the raw error rate is replaced by the area under the error rate curve. In that case, ratio \eqn{R} becomes \eqn{R = r(a+1) / r(a) - 1}.

- A "smoothed" version in which the raw error rate is smoothed (using function \code{\link{lowess}}) before calculating \eqn{R}.

Both variants can be usefull when the observed raw error rate is erratic, making difficult interpreting the \eqn{R} variations. This is frequent with classification error rates. 

}

\usage{
selncomp.wold(obj, nam = "rmsep", alpha = .01, 
  typ = c("raw", "integral", "smooth"), correct = TRUE, digits = 3,
  plot = TRUE, title = NULL, ...
  )
  }

\arguments{

\item{obj}{A dataframe containing at least two vectors describing the numbers of components (which has to be named \code{ncomp}) and the values of a given error rate \eqn{r}.}

\item{nam}{Name (character string) of the column corresponding to the error rate \eqn{r}. Default is \code{nam = "rmse"}.}

\item{typ}{Type of calculation of the criterium \eqn{R}. Possible values are \code{"raw"} (default), \code{"integral"} and \code{"smooth"}.}

\item{alpha}{Proportion \eqn{alpha} used as threshold for selecting the number of components.}

\item{correct}{Logical indicating if the selected number of components is corrected if it is higher than the number of components corresponding to the minimal error rate. Default to \code{TRUE}.}

\item{digits}{The number of digits for the numerical outputs.}

\item{plot}{Logical indicating is a plot is drawn. Default to \code{TRUE}.}

\item{title}{Optional title for the plot. Default to \code{NULL}.}

\item{...}{Other arguments to pass in function \code{\link{lowess}}.}

}

\references{

Wold S. Cross-Validatory Estimation of the Number of Components in Factor and Principal Components Models. Technometrics. 1978;20(4):397-405

}

\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

Xr <- detrend(Xr)

n <- nrow(Xr)
segm <- segmcvkfold(n = n, K = 5, typ = "random", nrep = 1)
fm <- fitcv(
  Xr, yr,
  fun = plsr,
  ncomp = 20,
  segm = segm,
  print = TRUE
  )
z <- mse(fm, ~ ncomp)
head(z)
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

u <- selncomp.wold(z, alpha = .01)
u$res
u$sel
u$opt

u <- selncomp.wold(z, typ = "integral", alpha = .1)
u$res
u$sel
u$opt

u <- selncomp.wold(z, typ = "smooth", f = 1/3)
u$res
u$sel
u$opt

}





\keyword{datagen}