\name{stahel}
\alias{stahel}
\encoding{latin1}

\title{Stahel-Donoho outlyingness}

\description{

The function computes the Stahel-Donoho outlyingness (Maronna & Yohai 1995, Hubert et al. 2005, Daszykowski et al. 2007) for a matrix \eqn{X}.

Outlyingness are calculated from the projections of the \eqn{n} data observations (rows of \eqn{X}) on a set of directions. The set of directions consists in the \eqn{n} data directions eventually completed by a number \eqn{nsim} of directions. In the function, the \eqn{nsim} directions are simulated as proposed in Hubert et al. (2005): random couples of observations are sampled in matrix \eqn{X} and, for each couple, the direction passes through the two observations of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

}

\usage{

stahel(X, scale = TRUE, nsim = 1000)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{scale}{If \code{TRUE} (default), matrix is preliminary robustly centered and scaled (by column-wise medians and MADs, respectively).}

\item{nsim}{A number of randomly simulated directions considered in addition to the\eqn{n} data observations (rows of \eqn{X}).}

}

\value{

A vector of outlyningness (length \eqn{n}).

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

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

stahel(X)


}

\keyword{datagen}