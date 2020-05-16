\name{stahel}
\alias{stahel}
\encoding{latin1}

\title{Stahel-Donoho outlyingness}

\description{

The function computes the Stahel-Donoho outlyingness (Maronna and Yohai, 1995) for a matrix \eqn{X}.

Outlyingness are calculated from the projections of the \eqn{n} data observations (rows of \eqn{X}) on a set of directions. This set of directions consists in the \eqn{n} data directions and a number of additionnal directions (argument \code{nsim}) simulated using the method of Hubert et al. (2005): random couples of observations are sampled and, for each couple, the direction passes through the two data points of the couple (see functions \code{.simpp.hub} in file \code{zfunctions.R}).

}

\usage{

stahel(X, scale = TRUE, nsim = 1000)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{scale}{If \code{TRUE} (default), matrix is preliminary robustly centered and scaled (by column-wise medians and MADs, respectively).}

\item{nsim}{A number of randomly simulated directions considered in addition to the\eqn{n} data observations (rows of \eqn{X}). Default to \code{nsim = 1000}.}

}

\value{

A vector of outlyningness (length \eqn{n}).

}

\references{

Daszykowski, M., Kaczmarek, K., Vander Heyden, Y., Walczak, B., 2007. Robust statistics in data analysis - A review. Chemometrics and Intelligent Laboratory Systems 85, 203-219. https://doi.org/10.1016/j.chemolab.2006.06.016

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