\name{covsel}
\alias{covsel}
\encoding{latin1}

\title{CovSel}

\description{

CovSel: Variable selection for highly multivariate and multi-response calibration (Roger et al. 2011).

Row observations can eventullay be weighted (not in the original article). 

}

\usage{

covsel(X, Y, nvar = NULL, scaly = TRUE, weights = NULL)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{A \eqn{n x q} matrix or data frame of responses. This can also be vector of length \eqn{n} if \eqn{Y} is univariate.}

\item{nvar}{The number of variables to select in \eqn{X}.}

\item{scaly}{If \code{TRUE} (default), each column of \eqn{Y} is scaled by its standard deviation.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

}

\value{

\item{sel}{A data.frame where variable \code{sel} shows the column numbers of the variables selected in \eqn{X}.}

\item{weights}{The weights used for the row observations.}

}

\references{

Roger, J.M., Palagos, B., Bertrand, D., Fernandez-Ahumada, E., 2011. CovSel: Variable selection for highly multivariate and multi-response calibration: Application to IR spectroscopy. Chem. Lab. Int. Syst. 106, 216-223. 

}


\examples{

n <- 6 ; p <- 4
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
Y <- matrix(rnorm(n * 2, mean = 10), ncol = 2, byrow = TRUE)
set.seed(NULL)
X
Y

covsel(X, Y, nvar = 3)

}

\keyword{datagen}