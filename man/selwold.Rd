\name{selwold}
\alias{selwold}
\encoding{latin1}

\title{Heuristic selection of the number of components in latent variable models with the Wold's criterion}

\description{

The function helps selecting the dimension (i.e. nb. components) of latent variable models (such as PCR, PLSR, PCDA, PLSDA, ...) using the "Wold criterion". 

The criterion is the "precision gain ratio" \eqn{R = 1 - r(a+1) / r(a)} where \eqn{r} is the observed error rate quantifying the model performance (rmsep, msep, classification error rate, etc.) and \eqn{a} the model dimension (= nb. components). 

\eqn{R} represents the relative gain in efficiency after a new dimension is added to the model. The iterations continue until \eqn{R} becomes lower than a threshold value \eqn{alpha}. By default and only as an indication, the default \eqn{alpha = 1} is set in the function, but the user should set any other value depending on his data and parcimony objective.

In the original article, for dimension selection, Wold (1978; see also Bro et al. 2008) used the ratio of \bold{cross-validated} vs. \bold{training} residual sums of squares (i.e. PRESS over SSR). Instead, function \code{selwold} only compares homogeneous quantities (the input vector \eqn{r}), such as PRESS values as in Li et al. (2002) and Andries et al. (2011). This last approach is equivalent to the "punish factor" described in Westad & Martens (2000).

In addition to \eqn{R}, function \code{selwold} also calculates delta values \eqn{diff = r(a+1) - r(a)}.

In some cases, particularly for classification (discrimination), the observed error rate \eqn{r} is erratic, making difficult the interpretation of the \eqn{R} variations. For such a situation, \code{selwold} proposes to calculate \eqn{R} and \eqn{diff} on two other possible values than the raw error rate \eqn{r}, using argument \code{typ}:

- \code{typ = "smooth"}. \eqn{R} and \eqn{diff} are calculated on a non-parametric smoothing of \eqn{r}. The smoothing is implemented with function \code{\link{lowess}}.

- \code{typ = "integral"}. \eqn{R}  and \eqn{diff} are calculated on the area under the observed error rate curve \eqn{r}. In this case, ratio \eqn{R} becomes \eqn{R = c.r(a+1) / c.r(a) - 1}, where \eqn{c.r} is the cumulated error rate ("area" under the curve).

Note that any other values than error rates (e.g. eigenvalues returned by a PCA) can be used as input \eqn{r} of \code{selwold}.

}

\usage{
selwold(r, start = 0, 
  type = c("raw", "smooth", "integral"), 
  alpha = .01, digits = 3,
  plot = c("R", "diff", "none"),
  xlab = "Index", ylab = "Value", main = "r",
  ...
  )
  }

\arguments{

\item{r}{A vector of a given error rate \eqn{r} (or any other value).}

\item{start}{Starting value for indexing the elements of vector \code{r} (default to 0). The index is returned in the output data.frame and the plots.}

\item{type}{Type of value used for calculating \eqn{R} and \eqn{diff}. Possible values are \code{"raw"} (default; calculations on \eqn{r}), \code{"smooth"} (on the smoothing of \eqn{r}) and \code{"integral"} (on the area under \eqn{r}).}

\item{alpha}{Proportion \eqn{alpha} used as threshold for \eqn{R}.}

\item{digits}{The number of digits for \eqn{R}.}

\item{plot}{Output plotted in the right side of the graphic window. Possible values are \code{"R"} (default), \code{"diff"} or \code{"none"} (no plot).}

\item{xlab}{x-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{ylab}{y-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{main}{Title of the plot of \eqn{r} (left-side in the graphic window).}

\item{...}{Other arguments to pass in function \code{\link{lowess}}.}

}

\value{

A list of outputs (see examples), such as:

\item{res}{Data.frame with variables: \code{r} (raw input \eqn{r}), \code{val} (eventually tranformed \eqn{r}), \code{diff} (calculated on \code{val}) and \code{R} (calculated on \code{val})..}

\item{opt}{The index of the minimum for \eqn{r}.}

\item{sel}{The index of the selection from the \eqn{R} threshold (usually a parcimonious number of components).}

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
fm <- cvfit(
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

u <- selwold(z$msep, alpha = .01,
    xlab = "Nb. components")
u$res
u$opt
u$sel

u <- selwold(z$msep, plot = "diff",
    xlab = "Nb. components")

u <- selwold(z$msep, type = "smooth", f = 1/3,  ## Smoothing not useful here
    xlab = "Nb. components")   
u$res
u$opt
u$sel

u <- selwold(z$msep, type = "integral", alpha = .05,
    xlab = "Nb. components")
u$res
u$opt
u$sel

}





\keyword{datagen}