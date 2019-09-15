\name{dis}
\alias{dis}
\encoding{latin1}

\title{Dissimilarities between row observations and a given vector}

\description{

Calculation of dissimilarities between the row observations of a data set and a given vector \eqn{mu}. By default, vector \eqn{mu} is the center (column means) of the reference (= training) data set.

}

\usage{

dis(Xr, mu = NULL, Xu = NULL, diss = c("euclidean", "mahalanobis", "correlation"),
  sigma = NULL, out = c("mad", "sd", "boxplot"), cri = 3)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{mu}{The vector of length \eqn{p} which is compared to the rows of \eqn{Xr} and, if not \code{NULL}, to the rows \eqn{Xu}. If \code{mu} is \code{NULL} (default), \code{mu} is set to the center (column means) of \eqn{Xr}. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations (\eqn{Xu} is not used in the calculation of the cutoff that defines the extreme dissimilarity values).}

\item{diss}{The type of dissimilarities used. Possible values are "euclidean" (Euclidean distances; default) or "mahalanobis" (Mahalanobis distances), or "correlation". Correlation dissimilarities are calculated by \eqn{sqrt(.5 * (1 - rho))}.}

\item{sigma}{For Mahalanobis distance, the covariance matrix. If \code{NULL} (default), this is the covariance matrix of \eqn{Xr}.}

\item{out}{Type of summary statistics used for calculating a cutoff for detecting extreme values of dissimilarity. Possible values are "mad" (mean absolute deviation "MAD"; default), "sd" (standard deviation) or "boxplot" (boxplot statistics). For "mad" and "sd", the cutoff is given by \eqn{median(d) + cri * mad(d)} or \eqn{mean(d) + cri * sd(d)}, respectively, where \eqn{d} is the vector of dissimilarities for the reference observations \eqn{Xr}. For "boxplot", the cutoff is given by \eqn{2nd quartile(d) + 1.5 * IQR(d)}.}

\item{cri}{Value used in the calculation of the "mad" or "sd" cutoff values for detecting the extreme dissimilarity values; see argument \code{out}.}

}

\details{

As indicator of extreme dissimilarity value, a "standardized" SD variable is calculated as \eqn{dstand = d / cutoff-value}. The dissimilarity is considered extreme if \eqn{dstand > 1}.

}

\value{

A list of outputs, such as:

\item{dr}{A data frame of the dissimilarities for Xr.}

\item{du}{A data frame of the dissimilarities for Xu.}

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
res <- dis(fm$Tr, diss = "mahalanobis")
names(res)
head(res$dr)

fm <- plsr(Xr, Yr, Xu, Yu, ncomp = 3)
res <- dis(fm$Tr, Xu = fm$Tu, diss = "mahalanobis")
names(res)
head(res$dr)
head(res$du)

}

\keyword{datagen}