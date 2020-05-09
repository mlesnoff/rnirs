\name{dasdod}
\alias{dasdod}
\encoding{latin1}

\title{DA using a SIMCA index}

\description{

Discriminant analysis using a SIMCA index (for comprehensive reviews, see e.g.: Vanden Branden & Hubert 2005, Daszykowski et al. 2007, Durante et al. 2011). 

For each new observation to predict, the function calculates a "SIMCA" index (output \eqn{index}) between the new observation and each of the classes of the reference (= training) data. The final predicted class corresponds to the class for which the index is the lowest. The principle is described below. 

A PCA is implemented for each class (= model representing the class). Two distances, referred to as SD and OD, are calculated for each new observation to predict and class. SD is the "score distance", i.e. the Mahalanobis distance between the projection of the new observation in the PCA score space and the center of this space. OD is the "orthogonal distance" (\eqn{X}-residual), i.e. the Euclidean distance between the new observation and its projection on the score space.

The SIMCA index used in function \code{dasdod} is \eqn{d = sqrt((1 - theta) * (SD / cutsd)^2 + theta * (OD / cutod)^2)}, where \eqn{cutsd} and \eqn{cutod} are cutoffs for the SD and OD distributions in the class, and \eqn{theta} is a proportion. Proportion \eqn{theta} is automatically varied from 0 ot 1 (with step=.1) in the function, and results are providen for each \eqn{theta}.

The values \eqn{SD / cutdsd} and \eqn{OD / cutdod} are "standardized" SD and OD, respectively. The test observations that show standardized SD or OD higher than 1 may be considered as outliers for the class. In a soft classification context, observations with standardized SD or OD lower than 1 may be considered as belonging to the class. 

The number of PCA components in each class is defined in argument \code{ncomp}. If they are not enough number of training observations in a given class, the number of components is automatically decreased (or the PCA is even cancelled if this number is lower than \code{nmim}). 

}


\usage{

dasdod(Xr, Yr, Xu, Yu = NULL, 
  ncomp, nmin = 5, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{ncomp}{Number of PCA components (i.e. scores) to be calculated for each class. A vector of same length as the number of classes, or an integer (in this last case, the same score number is given for all the classes). The number of scores is automatically decreased if the class size is too low.}

\item{nmin}{Minimal number of training observations in the class for implementing a PCA (default to \code{nmin = 5}). If this number is lower than \code{nmin}, the corresponding class level is not considered (a \code{NA} is returned for the SIMCA index of this level).}

\item{...}{Optionnal arguments to pass in function \code{\link{pca}}.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{index}{SIMCA index for the test data.}

\item{sdstand}{Standardized SD for the test data.}

\item{odstand}{Standardized OD for the test data.}

\item{cutsd}{Cutoff for calculating standardized SD.}

\item{cutod}{Cutoff for calculating standardized OD.}

\item{pvarcla}{Percentage of \eqn{X}-variance explained by the PCA.}

}


\references{

- Daszykowski, M., Kaczmarek, K., Stanimirova, I., Vander Heyden, Y., Walczak, B., 2007. Robust SIMCA-bounding influence of outliers. Chemometrics and Intelligent Laboratory Systems, 87, 95-103. https://doi.org/10.1016/j.chemolab.2006.10.003

- Durante, G., Bro, R., Cocchi, M. 2011. A classification tool for N-way array based on SIMCA methodology. Chem. Lab. Int. Syst., 106, 73-85.

- Vanden Branden, K., Hubert, M., 2005. Robust classification in high dimensions based on the SIMCA Method. Chem. Lab. Int. Syst., 79, 10-21.

}

\examples{

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

headm(Xr)
headm(Xu)

table(yr)
table(yu)

Xr <- snv(Xr)
Xu <- snv(Xu)

ncomp <- 15
fm <- dasdod(Xr, yr, Xu, yu, ncomp = ncomp, algo = pca.sph)
names(fm)
head(fm$y)
head(fm$fit)
head(fm$r)
head(fm$index)
head(fm$sd)
fm$cutsd
head(fm$sdstand)
head(fm$od)
fm$cutod
head(fm$odstand)
fm$ncomp
fm$pvarcla
fm$ni

err(fm, ~ theta)

}

\keyword{datagen}