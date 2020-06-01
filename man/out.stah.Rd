\name{out.stah}
\alias{out.stah}
\alias{out.eucl}
\alias{out.pca}
\encoding{latin1}

\title{Outlyingness measures}

\description{

Functions calculating outlyingness for the data observations (= rows of a matrix \eqn{X}). Outlyingness quantifies (in a relative scale) how far is an observation from the bulk (center) of the data. Such a measure can be used for detecting outliers and/or weighting in robust regression methods.

- \code{out.stah}: 

The function computes the Stahel-Donoho outlyingness (Maronna & Yohai 1995, Hubert et al. 2005, Daszykowski et al. 2007).

Outlyingness is calculated from the projections of the observation to a set of directions. The set of directions consists in the \eqn{n} directions corresponding to the rows of \eqn{X}, eventually completed by a number of \eqn{nsim} directions. In the function, the \eqn{nsim} directions are simulated as proposed in Hubert et al. (2005): random couples of observations are sampled in matrix \eqn{X} and, for each couple, the simulated direction is the one passing through the two observations of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

- \code{out.eucl}:

Outlyingness is calculated from the Euclidean distance between the observation and a robust estimate of the center of the data.

- \code{out.pca}:

Outlyingness is calculated from a given PCA space. A PCA is preliminary fitted with a given algorithm (ideally robust). Then, score (SD) and orthogonal (OD) distances are calculated, and standardized by cutoffs (see  \code{\link{scordis}} and  \code{\link{odis}}). The outlyingness is computed by \eqn{sqrt(.5 * SD_stand^2 + .5 * OD_stand^2)}. 

}

\usage{

out.stah(X, scale = TRUE, nsim = 1500)

out.eucl(X, scale = TRUE)

out.pca(X, ncomp, algo = pca.rob, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{scale}{If \code{TRUE} (default), for \code{out.stah}, matrix \eqn{X} is preliminary centered and scaled by column-wise medians and MADs, respectively, and for \code{out.eucl}, matrix \eqn{X} is preliminary scaled by column-wise MADs.}

\item{nsim}{For \code{out.stah}. A number of randomly simulated directions considered in addition to the \eqn{n} data observations (rows of \eqn{X}).}

\item{algo}{For \code{out.pca}. The algorithm function used for fitting the PCA.}

\item{ncomp}{For \code{out.pca}. The number of PCA scores (i.e. components) to be calculated.}

\item{...}{For \code{out.pca}. Optional arguments to pass in the function defined in \code{algo}.}

}

\value{

A vector of outlyningness (length \eqn{n}).

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

Hoffmann, I., Serneels, S., Filzmoser, P., Croux, C., 2015. Sparse partial robust M regression. Chemometrics and Intelligent Laboratory Systems 149, 50-59. https://doi.org/10.1016/j.chemolab.2015.09.019

Hubert, M., Rousseeuw, P.J., Vanden Branden, K., 2005. ROBPCA: A New Approach to Robust Principal Component Analysis. Technometrics 47, 64-79. https://doi.org/10.1198/004017004000000563

Maronna, R.A., Yohai, V.J., 1995. The Behavior of the Stahel-Donoho Robust Multivariate Estimator. Journal of the American Statistical Association 90, 330-341. https://doi.org/10.1080/01621459.1995.10476517

}

\examples{

n <- 6
p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
set.seed(NULL)
X

out.stah(X)
out.eucl(X)
out.pca(X, ncomp = 2)

}

\keyword{datagen}