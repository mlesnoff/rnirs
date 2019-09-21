\name{dasdod}
\alias{dasdod}
\encoding{latin1}

\title{DA using the SIMCA index}

\description{

Discriminant analysis using the SIMCA index (for comprehensive reviews, see e.g. Vanden Branden & Hubert 2005 and Durante et al. 2011). 

For each new observation to predict, the function calculates a SIMCA index \eqn{d} between the new observation and each of the classes of the reference (= training) data. The final predicted class corresponds to the class for which the index is the lowest. The principle is decsribed below. 

A PCA (= model representing the class) is implemented for each class. Then, two distances (referred to as SD and OD) are calculated for each new observation to predict and class. SD is the "score distance", i.e. the Mahalanobis distance between the projection of the new observation in the PCA score space and the center of the space. OD is the "orthogonal (or model) distance", i.e. the Euclidean distance between the new observation and its projection on the score space.

The SIMCA index used in function \code{dasdod} is \eqn{d = sqrt(.5 * (SD / c_sd)^2 + .5 * (OD / c_od)^2)}, where \eqn{c_sd} and \eqn{c_od} are scaling values making SD and OD comparable before aggregation. In the litterature, \eqn{c_sd} and \eqn{c_od} are often calculated from parametric asumptions (e.g. for SD, a \eqn{Chi-2} or Fisher distribution). Instead, function \code{dasdod} uses the robust non-parametric scaling values: 

- \eqn{c_sd = median(SD) + cri * mad(SD)}

- \eqn{c_od = median(OD) + cri * mad(OD)}

where \eqn{mad} is the mean absolute deviation ("MAD"), and \eqn{cri} is a scalar (by default \eqn{cri} = 3).

In the function, \eqn{c_sd} and \eqn{c_od} can be calculated using two different methods depending on argument \code{typcut}. The two methods are described below for \eqn{c_sd}. The scaling value \eqn{c_od} is calculated exactely in the same way.  Let us note \eqn{SD_ij} the score distance for an observation \eqn{i} (\eqn{i = 1, ..., n} for the training set, and \eqn{i = 1, ..., m} for the test set) and the class \eqn{j} (\eqn{j = 1, ..., L}). 

In the first method (default), all the score distances calculated for the test set are pooled over the \eqn{L} classes, giving the set \eqn{SD = {SD_ij ; i = 1, ..., m and j = 1, ..., L}} of size \eqn{m * L}. Then, a common scaling value is calculated as \eqn{c_sd = median(SD) + cri * mad(SD)}, and the SIMCA index for a new observation \eqn{i} and class \eqn{j} is \eqn{d_ij = sqrt(.5 * (SD_ij / c_sd)^2 + .5 * (OD_ij / c_od)^2)}.

In the second method, \eqn{c_sd} is calculated within each class, from the score distances distributions of the training set. For class \eqn{j}, the score distances calculated for the training set give the set \eqn{SD_j = {SD_ij ; i = 1, ..., n_j}}. Then, the scaling value is calculated as \eqn{c_sd_j = median(SD_j) + cri * mad(SD_j)}, and the SIMCA index for a new observation \eqn{i} is \eqn{d_ij = sqrt(.5 * (SD_ij / c_sd_j)^2 + .5 * (OD_ij / c_od_j)^2)}. This corresponds to the standard approach reported in the litterature except that c_sd_j is defined from parametric distributions.

By default in the function, the number of PCA components in the \eqn{L} classes is set consistent between the classes (if they are enough number of training observations in each of the classes; if not, the number of components for a given low-size class is automatically decreased). Alternatively, when using argument \code{pvar}, it is calculated independently for each class for reaching a given proportion of cumulated explained \eqn{X-}variance. When the number of PCA components is different between the classes, in the first method of calculation of the scaling values, SD_ij is divided by the number of components of class \eqn{j} for making comparable the values between the classes. 

}


\usage{
dasdod(Xr, Yr, Xu, Yu = NULL, ncomp.pls = NULL, ncomp.pca, nmin = 5,
  typcut = c("overall", "class"), pvar = NULL, cri = 3, ...)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp.pls}{If not \code{NULL}, a PLS is implemented on the original \eqn{X-}data and the PCAs per class are computed on the \code{ncomp.pls} components. This can improve discrimination on some data. Default to \code{NULL} (the PCAs are computed on the original data).}

\item{ncomp.pca}{The number of PCA components (i.e. scores) to be calculated for each class. Ignored if argument \code{pvar} is not \code{NULL}.}

\item{nmin}{Minimal number of training observations in the class for implementing a PCA. If this number is lower than \code{nmin}, the corresponding class level is not considered (a \code{NA} is returned for the SIMCA index of this level).}

\item{typcut}{Method for calculating the scaling values \eqn{c_sd} and \eqn{c_od} (see section \eqn{Description}). Possible values are "overall" (default; marginal calculation over all the classes) or "class" (calculation within each class).}

\item{pvar}{If not \code{NULL} (default), proportion of cumulated explained \eqn{X-}variance to reach in the calculation of the number of PCA components for each class.}

\item{cri}{Value used in the calculation of the "mad" cutoffs for defining the scaling values \eqn{c_sd} and \eqn{c_od}.}


\item{...}{Optionnal arguments to pass in function \code{\link{pca}}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}


\references{

- Durante, G., Bro, R., Cocchi, M. 2011. A classification tool for N-way array based on SIMCA methodology. Chem. Lab. Int. Syst., 106, 73-85.

- Vanden Branden, K., Hubert, M., 2005. Robust classification in high dimensions based on the SIMCA Method. Chem. Lab. Int. Syst., 79, 10-21.

}

\examples{

data(iris)

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
dim(Xr)
dim(Xu)

table(yr)
table(yu)

ncomp.pca <- 15
fm <- dasdod(Xr, yr, Xu, yu, ncomp.pca = ncomp.pca)
fm$ncompcla
fm$pvarcla
err(fm)
err(fm, ~ y1)

fm <- dasdod(Xr, yr, Xu, yu, ncomp.pls = 20, ncomp.pca = 15)
fm$ncompcla
fm$pvarcla
err(fm)

}

\keyword{datagen}