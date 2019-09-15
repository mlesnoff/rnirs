\name{odis}
\alias{odis}
\encoding{latin1}

\title{Orthogonal distances from a PCA or PLS score space}

\description{

Calculation of the Euclidean orthogonal distances (OD = "\eqn{X}-residuals") between row observations of a data set and their projection on a PCA or PLS score space (e.g. Hubert et al. 2005, Van Branden & Hubert 2005, p. 66; Varmuza & Filzmoser, 2009, p. 79).

OD are calculated from a preliminary calculated PCA or PLS.

}

\usage{

odis(Xr, fm, Xu = NULL, ncomp = NULL, out = c("mad", "sd", "boxplot"), cri = 3)

}

\arguments{

\item{Xr}{The matrix or data frame of reference (= training) observations that was used for building the preliminary model \code{fm}.}

\item{fm}{Output of one of the functions implementing PCA, PLS, etc.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations (\eqn{Xu} is not used in the calculation of the cutoff that defines the extreme distance values).}

\item{ncomp}{Number of components defining the score space to consider for calculating the distances. Default to \code{NULL} (\code{ncomp} is set to the maximal number of scores defined in \code{fm}).}

\item{out}{Type of summary statistics used for detecting extreme OD values. Possible values are "mad" (mean absolute deviation "MAD"; default), "sd" (standard deviation) or "boxplot" (boxplot statistics). For "mad" and "sd", the cutoff is given by \eqn{median(d) + cri * mad(d)} or \eqn{mean(d) + cri * sd(d)}, respectively, where \eqn{d} is the vector of dissimilarities for the reference observations \eqn{Xr}. For "boxplot", the cutoff is given by \eqn{2nd quartile(d) + 1.5 * IQR(d)}.}

\item{cri}{Value used in the calculation of the "mad" or "sd" cutoff values for detecting the extreme distance values; see argument \code{out}.}

}

\details{

As indicator of extreme distance value, a "standardized" OD variable is calculated as \eqn{dstand = d / cutoff-value}. The OD is considered extreme if \eqn{dstand > 1}.

}

\references{
M. Hubert, P. J. Rousseeuw, K. Vanden Branden (2005). ROBPCA: a new approach to robust principal components analysis. Technometrics, 47, 64-79.

K. Vanden Branden, M. Hubert (2005). Robuts classification in high dimension based on the SIMCA method. Chem. Lab. Int. Syst, 79, 10-21. 

K. Varmuza, P. Filzmoser (2009). Introduction to multivariate statistical analysis in chemometrics. CRC Press, Boca Raton.
}

\examples{

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)
Xr <- X[1:6, ] ; Yr <- Y[1:6, ]
Xu <- X[7:8, ] ; Yu <- Y[7:8, ]

fm <- pls(Xr, Yr, ncomp = 3)
res <- odis(Xr, fm)
names(res)
head(res$dr)

fm <- plsr(Xr, Yr, Xu, Yu, ncomp = 3)
res <- odis(Xr, fm, Xu)
names(res)
head(res$dr)
head(res$du)

}

\keyword{datagen}