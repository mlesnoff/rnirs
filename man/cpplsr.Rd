\name{cpplsr}
\alias{cpplsr}
\encoding{latin1}

\title{Mallows Cp for PLSR1 Models}

\description{

Calculation of the Mallows \eqn{Cp} criterion for PLSR1 models. For a model with \eqn{a} components, function \code{cpplsr} calculates \eqn{Cp} by:

\eqn{Cp(a) = SSR(a) / n + alpha * df(a) * sigma^2 / n}  

where \eqn{SSR} is the sum of squared residuals for the current model, \eqn{df(a)} the estimated PLSR model complexity (nb. model's degrees of freedom), \eqn{sigma^2} the irreductible error variance estimated from a low biased model, \eqn{alpha} a penalty coefficient, and \eqn{n} the number of training observations.  

\bold{## Penalty coefficient \eqn{alpha}}

Depending on argument \code{type}, \eqn{alpha} is equal to either \eqn{2} (AIC penalty), \eqn{2 * n / (n - df - 1)} (small sample size correction AICc penalty) or \eqn{log(n)} (BIC penalty).

\bold{## Nb. model's degrees of freedom df}

Depending on argument \code{methdf}, model complexity \eqn{df} can be estimated 

- from fonctions \code{\link{dfplsr.cov}} or \code{\link{dfplsr.div}}, 

- or from the crude rule of thumb \eqn{1 + theta * a} where \eqn{theta} is a given scalar higher or equal to 1 (\eqn{theta = 1} is the naive \eqn{df} estimation ; in general, it generates significant underestimations of \eqn{Cp}.).

}

\usage{

cpplsr(X, Y, ncomp, algo = NULL,
  type = c("aicc", "aic", "bic"),
  methdf = c("cov", "div", "naive"),
  B = 50, eps = 1e-4, seed = NULL,
  theta = 3, 
  print = TRUE, ...) 

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{Y}{A vector of length \eqn{n} of training responses. }

\item{ncomp}{The maximal number of PLS scores (= components = latent variables) to consider.}

\item{algo}{a PLS algorithm. Default to  \code{NULL} (\code{\link{pls.kernel}} is used).}

\item{methdf}{The method used for estimating \eqn{df}. Possible values are \code{"cov"} (default), \code{"div"} or \code{"naive"}.}

\item{B}{For \code{methdf = "cov"}: the number of bootstrap replications (see \code{\link{dfplsr.cov}}). For \code{methdf = "dif"}: the number of observations in the data receiving perturbation (maximum is \eqn{n}; see \code{\link{dfplsr.cov}}).}

\item{type}{Type of penalty coefficient. Possible values are \code{"aicc"} (default), \code{"aic"} or \code{"bic"}. See \bold{Description} section.}

\item{eps}{For \code{methdf = "dif"}. The \eqn{epsilon} quantity used for scaling the perturbation analysis (see \code{dfplsr.div}).}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{theta}{A scalar used in the crude estimation of \eqn{df}.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

Several items. In particular, a data.frame with the estimated \eqn{Cp} and corresponding model weights (so-called "Akaike weights").
}

\references{

Burnham, K.P., Anderson, D.R., 2002. Model selection and multimodel inference: a practical informationtheoretic approach, 2nd ed. Springer, New York, NY, USA.

Burnham, K.P., Anderson, D.R., 2004. Multimodel Inference: Understanding AIC and BIC in Model
Selection. Sociological Methods & Research 33, 261–304. https://doi.org/10.1177/0049124104268644

Efron, B., 2004. The Estimation of Prediction Error. Journal of the American Statistical Association 99,
619–632. https://doi.org/10.1198/016214504000000692

Eubank, R.L., 1999. Nonparametric Regression and Spline Smoothing, 2nd ed, Statistics: Textbooks
and Monographs. Marcel Dekker, Inc., New York, USA.

Hastie, T., Tibshirani, R.J., 1990. Generalized Additive Models, Monographs on statistics and applied
probablity. Chapman and Hall/CRC, New York, USA.

Hastie, T., Tibshirani, R., Friedman, J., 2009. The elements of statistical learning: data mining,
inference, and prediction, 2nd ed. Springer, NewYork.

Hastie, T., Tibshirani, R., Wainwright, M., 2015. Statistical Learning with Sparsity: The Lasso and
Generalizations. CRC Press

Hurvich, C.M., Tsai, C.-L., 1989. Regression and Time Series Model Selection in Small Samples. Biometrika
76, 297. https://doi.org/10.2307/2336663

Mallows, C.L., 1973. Some Comments on Cp. Technometrics 15, 661–675.
https://doi.org/10.1080/00401706.1973.10489103

Ye, J., 1998. On Measuring and Correcting the Effects of Data Mining and Model Selection. Journal of
the American Statistical Association 93, 120–131. https://doi.org/10.1080/01621459.1998.10474094

Zuccaro, C., 1992. Mallows’ Cp Statistic and Model Selection in Multiple Linear Regression.
International Journal of Market Research. 34, 1–10. https://doi.org/10.1177/147078539203400204

}

\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

ncomp <- 20
B <- 30
fm <- cpplsr(Xr, yr, ncomp = ncomp, 
            type = "aicc",
            methdf = "cov", B = B,
            )
names(fm)
z <- fm$res
u <- selwold(z$crit[-1], start = 1,
             xlab = "Nb. components", main = "Cp", alpha = 0)

}

\keyword{datagen}