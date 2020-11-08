\name{selwold}
\alias{selwold}
\encoding{latin1}

\title{Heuristic selection of the number of components in a PLSR or PLSDA model with the Wold's criterion}

\description{

The function helps to select the number of components in a PLSR or PLSDA model using a heuristic criterion (Wold, 1978). 

The criterion is \eqn{R = 1 - r(a+1) / r(a)} where \eqn{r} is the observed error rate quantifying the model performance (rmsep, msep, classification error rate, etc.) and \eqn{a} the model dimension (= nb. components). \eqn{R} represents the relative gain in efficiency after a new dimension is added to the model. The iterations continue until \eqn{R} becomes lower than a threshold value \eqn{alpha}. By default and as an indication, \eqn{alpha} is set to .01 in the function, but the user should set any more relevant value for his data, depending on his objective of parcimony.

In some cases, the observed error rate \eqn{r} is erratic, making difficult interpreting the \eqn{R} variations. This is for instance frequent with the classification error rate used in discrimination. For such a situation, \code{selwold} proposes two alternative to the observed error rate \eqn{r} (using argument \code{typ}):

- \code{typ = "integral"}. \eqn{R} is calculated from the area under the observed error rate curve \eqn{r}. In that case, ratio \eqn{R} becomes \eqn{R = c.r(a+1) / c.r(a) - 1}, where \eqn{c.r} is the cumulated error rate ("area" under the curve).

- \code{typ = "smoothed"}. \eqn{R} is calculated on a non-parametric smoothing  (using function \code{\link{lowess}}) of \eqn{r}.

}

\usage{
selwold(obj, nam = "msep", alpha = .01, 
  typ = c("raw", "smooth", "integral"), 
  correct = TRUE, digits = 3, plot = TRUE, ...
  )
  }

\arguments{

\item{obj}{A dataframe containing at least two vectors describing the numbers of components (which has to be named \code{ncomp}) and the values of a given error rate \eqn{r}.}

\item{nam}{Name (character string) of the column corresponding to the error rate \eqn{r}. Default is \code{nam = "msep"}.}

\item{typ}{Type of calculation of the criterium \eqn{R}. Possible values are \code{"raw"} (default; usual calculation on the observed error rate), \code{"smooth"} and \code{"integral"}.}

\item{alpha}{Proportion \eqn{alpha} used as threshold for selecting the number of components.}

\item{correct}{Logical indicating if the selected number of components is corrected if it is higher than the number of components corresponding to the minimal error rate. Default to \code{TRUE}.}

\item{digits}{The number of digits for the numerical outputs.}

\item{plot}{Logical indicating is a plot is dobservedn. Default to \code{TRUE}.}

\item{...}{Other arguments to pass in function \code{\link{lowess}}.}

}

\references{

Wold S. Cross-Validatory Estimation of the Number of Components in Factor and Principal Components Models. Technometrics. 1978;20(4):397-405

}

\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

n <- nrow(Xr)
segm <- segmkf(n = n, K = 5, typ = "random", nrep = 1)
fm <- fitcv(
  Xr, yr,
  fun = plsr,
  ncomp = 20,
  segm = segm,
  print = TRUE
  )

z <- mse(fm, ~ ncomp)
head(z)
z[z$msep == min(z$msep), ]
plotmse(z)

u <- selwold(z, alpha = .01)
u$res
u$sel
u$opt

u <- selwold(z, typ = "integral", alpha = .05)
u$res
u$sel
u$opt

u <- selwold(z, typ = "smooth", f = 1/2)   ## Smoothing not useful here
u$res
u$sel
u$opt

}





\keyword{datagen}