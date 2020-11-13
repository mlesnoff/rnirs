\name{selwold}
\alias{selwold}
\encoding{latin1}

\title{Heuristic selection of the number of components in a PCR/PLSR or PCDA/PLSDA model with the Wold's criterion}

\description{

The function helps to select the number of components in a PCR/PLSR or PCDA/PLSDA model using the heuristic "Wold criterion". 

The criterion is the "precision gain ratio" \eqn{R = 1 - r(a+1) / r(a)} where \eqn{r} is the observed error rate quantifying the model performance (rmsep, msep, classification error rate, etc.) and \eqn{a} the model dimension (= nb. components). 

\eqn{R} represents the relative gain in efficiency after a new dimension is added to the model. The iterations continue until \eqn{R} becomes lower than a threshold value \eqn{alpha}. By default and as an indication, \eqn{alpha} is set to .01 in the function, but the user should set any more relevant value for his data, depending on his objective of parcimony.

In the original article, Wold (1978; see also Bro et al. 2008) defined ratios comparing training vs. CV residual sums of squares (SSR and PRESS, respectively). Instead, function \code{selwold} compares homogeneous residual sums of squares, usually the PRESS, such as for instance in Li et al. (2002) and Andries et al. (2011). This homogeneous approach is equivalent to the "punish factor" described in Westad & Martens (2000).  

In some cases, the observed error rate \eqn{r} is erratic, making difficult interpreting the \eqn{R} variations. In particular, this often occurs for classification error rates (discrimination). For such a situation, \code{selwold} proposes two better alternative to the observed error rate \eqn{r}, using argument \code{typ}:

- \code{typ = "integral"}. \eqn{R} is calculated from the area under the observed error rate curve \eqn{r}. In that case, ratio \eqn{R} becomes \eqn{R = c.r(a+1) / c.r(a) - 1}, where \eqn{c.r} is the cumulated error rate ("area" under the curve).

- \code{typ = "smoothed"}. \eqn{R} is calculated on a non-parametric smoothing of \eqn{r}. The smoothing is implemented with function \code{\link{lowess}}.

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

Andries, J.P.M., Vander Heyden, Y., Buydens, L.M.C., 2011. Improved variable reduction in partial least squares modelling based on Predictive-Property-Ranked Variables and adaptation of partial least squares complexity. Analytica Chimica Acta 705, 292-305. https://doi.org/10.1016/j.aca.2011.06.037

Bro, R., Kjeldahl, K., Smilde, A.K., Kiers, H.A.L., 2008. Cross-validation of component models: A critical look at current methods. Anal Bioanal Chem 390, 1241-1251. https://doi.org/10.1007/s00216-007-1790-1

Li, B., Morris, J., Martin, E.B., 2002. Model selection for partial least squares regression. Chemometrics and Intelligent Laboratory Systems 64, 79-89. https://doi.org/10.1016/S0169-7439(02)00051-5

Westad, F., Martens, H., 2000. Variable Selection in near Infrared Spectroscopy Based on Significance Testing in Partial Least Squares Regression. J. Near Infrared Spectrosc., JNIRS 8, 117–124.

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