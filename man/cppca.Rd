\name{cppca}
\alias{cppca}
\encoding{latin1}

\title{Mallows Cp for PCA Models}

\description{

Calculation of the Mallows \eqn{Cp} (Mallows 1973) criterion for PCA models. For a model with \eqn{a} components and a matrix \eqn{X} of dimension \eqn{n x p}, function \code{cppca} calculates \eqn{Cp} by:

\eqn{Cp(a) = SSR(a) / N + alpha * dfc(a) * sigma^2 / N}  

where \eqn{SSR} is the sum of squared residuals for the current PCA model, \eqn{dfc(a)} an estimation of the PCA "corrected model complexity" (see thereafter), \eqn{sigma^2} the irreductible error variance estimated from a low biased model, \eqn{alpha} a penalty coefficient, and \eqn{N = n * p} the number of (training) matrix elements.

\bold{## Penalty coefficient \eqn{alpha}}

Depending on argument \code{type}, \eqn{alpha} is equal to either \eqn{2} (AIC penalty), \eqn{2 * N / (N - df - 1)} (small sample size correction AICc penalty) or \eqn{log(N)} (BIC penalty).

\bold{## Corrected nb. model's degrees of freedom \eqn{dfc}}

The model complexity of a fitted PCA model of dimension \eqn{a} is known to be \eqn{df = p + a * (n - 1) + p * a - a^2} (Faber et al. 1994, Faber 2012, Josse & Husson 2012). Monte Carlo estimates (Ye 1998, Efron 2004) of \eqn{df} are consistent with this previous formula; see for instance the example presented for function \code{\link{dfpca.div}}. 

Nevertheless and unfortunately, \eqn{df} can not be used directly in criteria such as \eqn{Cp}, \eqn{GCV} etc. for model selection. Such a procedure underestimates the prediction error that is normally targetted by these criteria, with the consequence of selecting models with too large dimensions. 

The underestimation comes from the fact that, in PCA, the new observations to predict, say \eqn{Xnew}, are directly used for computing the predictions \eqn{Xnew_fit} (\eqn{Xnew_fit = Xnew * P * P'}). Objects \eqn{Xnew} and \eqn{Xnew_fit} are therefore not independant, while such an independance is a fundamental hypothesis of the theory leading to the good properties of \eqn{Cp} and \eqn{GCV}. As a comparison, in more usual supervised prediction models such as PLSR models where \eqn{Cp} etc. are used, objects \eqn{ynew} and \eqn{ynew_fit} are independant.

The non-independance between \eqn{Xnew} and \eqn{Xnew_fit} consumes specific degrees of freedom that need to be added to \eqn{df} before using the Cp formula. Let us define the \bold{corrected} number of degrees of freedom (used finally in the \eqn{Cp} formula) by \eqn{dfc = df + theta}, where \eqn{theta} corresponds to the additional part. Function \eqn{cppca} estimates \eqn{dfc} from the following rule of thumb:

the ratio \eqn{N / (N - dfc)} is assumed being approximately equal to the ratio \eqn{SSRCV / SSR}, where \eqn{dfc} is unkwown and \eqn{SSRCV} is the \eqn{PRESS} returned by a cross-validation (CV) process. 

This rule of thumb was set here after looking at the nice idea initiated by Hassani et al. 2012. But in our case, the additional part \eqn{theta} has different interpretation and justification: we claim that \eqn{theta} comes frop the non-independance between \eqn{Xnew} and \eqn{Xnew_fit}, not from some correlation cost of computing PCA loadings ans scores (which is, for us, already accounted for in \eqn{df}).  

Following this, it comes that 

\eqn{dfc = N * (1 - SSR / SSRCV)}

}

\usage{

cppca(X, ncomp, algo = NULL,
  segm = segm,
  typ = c("aicc", "aic", "bic"), 
  k = 10,
  print = TRUE, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{ncomp}{The maximal number of scores (= components = latent variables) to consider.}

\item{algo}{a PCA algorithm. Default to  \code{NULL} (see \code{\link{pca}}).}

\item{type}{Type of penalty coefficient. Possible values are \code{"aicc"} (default), \code{"aic"} or \code{"bic"}. See \bold{Description} section.}

\item{k}{Dimension of the PCA model used for estimating \code{"sigma2"}. In general, a value between 10 and 15 seems relevant.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

Several items. In particular, a data.frame with the estimated \eqn{Cp} and corresponding model weights (so-called "Akaike weights").

}

\references{

Efron, B., 2004. The Estimation of Prediction Error. Journal of the American Statistical Association 99,
619–632. https://doi.org/10.1198/016214504000000692

Hassani, S., Martens, H., Qannari, E.M., Kohler, A., 2012. Degrees of freedom estimation in Principal Component Analysis and Consensus Principal Component Analysis. Chemometrics and Intelligent Laboratory Systems 118, 246-259. https://doi.org/10.1016/j.chemolab.2012.05.015

Faber, N.M., Buydens, L.M.C., Kateman, G., 1994. Aspects of pseudorank estimation methods based on the eigenvalues of principal component analysis of random matrices. Chemometrics and Intelligent Laboratory Systems 25, 203–226. https://doi.org/10.1016/0169-7439(94)85043-7

Faber, N. (Klaas) M., 2008. Degrees of freedom for the residuals of a principal component analysis — A clarification. Chemometrics and Intelligent Laboratory Systems 93, 80–86. https://doi.org/10.1016/j.chemolab.2008.04.006

Josse, J., Husson, F., 2012. Selecting the number of components in principal component analysis using cross-validation approximations. Computational Statistics & Data Analysis 56, 1869–1879. https://doi.org/10.1016/j.csda.2011.11.012

Mallows, C.L., 1973. Some Comments on Cp. Technometrics 15, 661–675.
https://doi.org/10.1080/00401706.1973.10489103

Ye, J., 1998. On Measuring and Correcting the Effects of Data Mining and Model Selection. Journal of
the American Statistical Association 93, 120–131. https://doi.org/10.1080/01621459.1998.10474094

}

\examples{

data(datoctane)
X <- datoctane$X
## removing outliers
zX <- X[-c(25:26, 36:39), ]
n <- nrow(zX)
p <- ncol(zX)
N <- n * p
plotsp(zX)

K <- 5
segm <- segmkf(n = n, K = K, nrep = 1, seed = 1)
ncomp <- 15
fm <- cppca(zX, ncomp = ncomp, 
  segm = segm,
  type = "aicc"
  )
names(fm)
z <- fm$res
u <- selwold(z$crit[-1], start = 1,
             xlab = "Nb. components", main = "Cp", alpha = 0)

}

\keyword{datagen}