\name{plsrdf.cov}
\alias{plsrdf.cov}
\alias{plsrdf.div}
\encoding{latin1}

\title{Degrees of freedom of PLSR1 Models}

\description{

Monte Carlo methods for estimating the model complexity \eqn{df} (number of degrees of freedom) of PLSR1 models.

- In \code{plsrdf.cov}, the covariances presented by Efron 2004 (Eq. 2.16) are calculated  by parametric bootstrap. The residual variance \eqn{sigma^2} is estimated from a low-biased model.

- In \code{plsrdf.div}, the divergencies \eqn{dy_fit/dy} presented by Ye (1998) and Efron (2004) are calculated by perturbation analysis. This is a "Stein unbiased risk estimation" (SURE) of \eqn{df}.

}

\usage{

plsrdf.cov(X, Y, ncomp, algo = NULL,
  B = 50,
  seed = NULL, print = TRUE, ...)

plsrdf.div(X, Y, ncomp, algo = NULL, 
  ns = 10, meth.samp = c("syst", "random"), eps = 1e-4, 
  seed = NULL, print = TRUE, ...) 

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{Y}{A vector of length \eqn{n} of training responses. }

\item{ncomp}{The maximal number of PLS scores (= components = latent variables) to consider.}

\item{algo}{a PLS algorithm. Default to  \code{NULL} (\code{\link{pls.kernel}} is used).}

\item{B}{For \code{plsrdf.cov}. Number of bootstrap replications.}

\item{ns}{For \code{plsrdf.div}. Number of observations in the data receiving perturbation. The maximum is \eqn{n}.}

\item{meth.samp}{For \code{plsrdf.div}. When \eqn{ns < n}, method used for sampling the observations receiving a perturbation. Possible values are "syst" (default; observations are sampled regularly over \eqn{y}) or "random".}

\item{eps}{For \code{plsrdf.div}. The \eqn{epsilon} quantity used for scaling the perturbation analysis.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs (see examples), such as:

\item{df}{The model complexity for the models with \eqn{a = 0, 1, ..., ncomp} components.}

}

\references{

Efron, B., 2004. The Estimation of Prediction Error. Journal of the American Statistical Association 99,
619–632. https://doi.org/10.1198/016214504000000692

Hastie, T., Tibshirani, R.J., 1990. Generalized Additive Models, Monographs on statistics and applied
probablity. Chapman and Hall/CRC, New York, USA.

Hastie, T., Tibshirani, R., Friedman, J., 2009. The elements of statistical learning: data mining,
inference, and prediction, 2nd ed. Springer, NewYork.

Hastie, T., Tibshirani, R., Wainwright, M., 2015. Statistical Learning with Sparsity: The Lasso and
Generalizations. CRC Press

Kramer, N., Braun, M.L., 2007. Kernelizing PLS, degrees of freedom, and efficient model selection, in: Proceedings of the 24th International Conference on Machine Learning, ICML 07. Association for Computing Machinery, New York, NY, USA, pp. 441-448. https://doi.org/10.1145/1273496.1273552

Kramer, N., Sugiyama, M., 2011. The Degrees of Freedom of Partial Least Squares Regression. Journal of the American Statistical Association 106, 697-705. https://doi.org/10.1198/jasa.2011.tm10107

Kramer, N., Braun, M. L. 2019. plsdof: Degrees of Freedom and Statistical Inference for Partial Least Squares Regression. R package version 0.2-9. https://cran.r-project.org

Stein, C.M., 1981. Estimation of the Mean of a Multivariate Normal
Distribution. The Annals of Statistics 9, 1135–1151.

Ye, J., 1998. On Measuring and Correcting the Effects of Data Mining and Model Selection. Journal of
the American Statistical Association 93, 120–131. https://doi.org/10.1080/01621459.1998.10474094

Zou, H., Hastie, T., Tibshirani, R., 2007. On the “degrees of freedom” of the lasso. The Annals of
Statistics 35, 2173–2192. https://doi.org/10.1214/009053607000000127

}

\examples{

## Below is an example reproducing the numerical illustration
## given by Kramer & Sugiyama 2011 (Fig. 1, center) 
## on the Ozone data.
## Note that function "pls.model" of the R package "plsdof" v0.2-9 (Kramer & Braun 2019)
## automatically scales the X matrix before PLS.

data(datozone)

z <- datozone$X
u <- which(!is.na(rowSums(z)))    ## Removing rows with NAs
X <- z[u, -4]
y <- z[u, 4]
dim(X)

zX <- scale(X)    ## Scaling for consistency with plsdof

ncomp <- 12
ns <- B <- 50   ## Should be increased for more stability
u <- plsrdf.cov(zX, y, ncomp = ncomp, B = B)
v <- plsrdf.div(zX, y, ncomp = ncomp, ns = ns)

plot(0:ncomp, u$df, type = "b", col = "red",
     ylim = c(0, 15),
     xlab = "Nb components", ylab = "df")
lines(0:ncomp, v$df, col = "blue", type = "b")                 
lines(0:ncomp, zncomp + 1, col = "grey40")   ## Naive df
abline(h = 1, lty = 2, col = "grey")

}

\keyword{datagen}