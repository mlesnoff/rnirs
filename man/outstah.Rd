\name{outstah}
\alias{outstah}
\alias{outeucl}
\alias{outsdod}
\encoding{latin1}

\title{Outlyingness measures}

\description{

Functions calculating outlyingness for the data observations (= rows of a matrix \eqn{X}). Outlyingness quantifies (in a relative scale) how far is an observation from the bulk (center) of the data. Such a measure can be used for detecting outliers and/or weighting the row observatiosn in robust regression methods.

- \code{outstah}: 

The function computes the Stahel-Donoho outlyingness (Maronna & Yohai 1995, Hubert et al. 2005, Daszykowski et al. 2007).

Outlyingness is calculated from the projections of the observation to a set of directions. The set of directions consists in the \eqn{n} directions corresponding to the rows of \eqn{X}, eventually completed by a number of \eqn{nsim} directions. In the function, the \eqn{nsim} directions are simulated as proposed in Hubert et al. (2005): random couples of observations are sampled in matrix \eqn{X} and, for each couple, the simulated direction is the one passing through the two observations of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

- \code{outeucl}:

Outlyingness is calculated by the Euclidean distance between the observation and a robust estimate of the center of the data (either the column-wise median or the spatial median). The euclidean distance is then scaled by the median of the \eqn{n} calculated Euclidean distances. Such outlyingness was for instance used in the robust PLSR (PRM) algorithm of Serneels et al. 2005.   

- \code{outsdod}:

Outlyingness is calculated from a fitted score space. For instance, a PCA (or PLS) is preliminary fitted with a given algorithm (ideally robust). Then, score (SD) and orthogonal (OD) distances are calculated for the fitted score space and standardized by cutoffs (see  \code{\link{scordis}} and  \code{\link{odis}}). The outlyingness is then computed by \eqn{sqrt(.5 * SD_stand^2 + .5 * OD_stand^2)}. 

}

\usage{

outstah(X, scale = TRUE, nsim = 1500)

outeucl(X, scale = TRUE, spatial = FALSE)

outsdod(fm, X, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{scale}{If \code{TRUE} (default), for \code{outstah}, matrix \eqn{X} is preliminary centered and scaled by column-wise medians and MADs, respectively, and for \code{outeucl}, matrix \eqn{X} is preliminary scaled by column-wise MADs.}

\item{nsim}{For \code{outstah}. A number of randomly simulated directions considered in addition to the \eqn{n} data observations (rows of \eqn{X}).}

\item{spatial}{For \code{outeucl}. Logical. If \code{TRUE}, the center of the data is calculated by the spatial median (computed by function \code{.xmedspa} available in file \code{zfunctions.R}; this function uses the fast code of rrcov v.1.4-3 available on R CRAN; Thanks to V. Todorov, 2016). If \code{FALSE} (default), the center of the data is calculated by the column-wise medians.}

\item{fm}{For \code{outsdod}. The PCA model from wich is calculated the outlyingness.}

\item{...}{For \code{outsdod}. Optional arguments to pass in functions \code{\link{scordis}} and  \code{\link{odis}}.}

}

\value{

A vector of outlyningness (length \eqn{n}).

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

Hoffmann, I., Serneels, S., Filzmoser, P., Croux, C., 2015. Sparse partial robust M regression. Chemometrics and Intelligent Laboratory Systems 149, 50-59. https://doi.org/10.1016/j.chemolab.2015.09.019

Hubert, M., Rousseeuw, P.J., Vanden Branden, K., 2005. ROBPCA: A New Approach to Robust Principal Component Analysis. Technometrics 47, 64-79. https://doi.org/10.1198/004017004000000563

Maronna, R.A., Yohai, V.J., 1995. The Behavior of the Stahel-Donoho Robust Multivariate Estimator. Journal of the American Statistical Association 90, 330-341. https://doi.org/10.1080/01621459.1995.10476517

Serneels, S., Croux, C., Filzmoser, P., Van Espen, P.J., 2005. Partial robust M-regression. Chemometrics and Intelligent Laboratory Systems 79, 55-64. https://doi.org/10.1016/j.chemolab.2005.04.007

}

\examples{

n <- 6
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
X

outstah(X)
outeucl(X)

fm <- pca.rob(X, ncomp = 2)
outsdod(fm, X)

}

\keyword{datagen}