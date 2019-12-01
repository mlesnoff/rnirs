\name{scordis}
\alias{scordis}
\encoding{latin1}

\title{Score distances (SD)}

\description{

\code{scordis} calculates score distances (SD) from a PCA, PLS or PLSR model, i.e. the Mahalanobis distances between the projections of the row observations on the score space and the center of the score space.

}

\usage{

scordis(fm, out = c("mad", "sd", "boxplot"), cri = 3)

}

\arguments{

\item{fm}{Output of functions \code{\link{pca}}, \code{\link{pls}} or \code{\link{plsr}}.}

\item{out}{Type of summary statistics used for detecting extreme SD values. Possible values are "mad" (mean absolute deviation "MAD"; default), "sd" (standard deviation) or "boxplot" (boxplot statistics). For "mad" and "sd", the cutoff is given by \eqn{median(d) + cri * mad(d)} or \eqn{mean(d) + cri * sd(d)}, respectively, where \eqn{d} is the vector of dissimilarities for the reference observations \eqn{Xr}. For "boxplot", the cutoff is given by \eqn{2nd quartile(d) + 1.5 * IQR(d)}.}

\item{cri}{Value used in the calculation of the "mad" or "sd" cutoff values for detecting the extreme distance values; see argument \code{out}.}

}

\details{

The "standardized" variable \eqn{dstand} calculated as \eqn{dstand = d / cutoff-value} is an  indicator of extreme value. SD is considered extreme if \eqn{dstand > 1}. The Winisi GH is also provided (extreme if GH > 3).

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

fm <- pca(Xr, ncomp = 3)
#fm <- pls(Xr, Yr, ncomp = 3)
scordis(fm)

fm <- pca(Xr, Xu, ncomp = 3)
#fm <- pls(Xr, Yr, Xu, ncomp = 3)
#fm <- plsr(Xr, Yr, Xu, ncomp = 3)
scordis(fm)

}

\keyword{datagen}