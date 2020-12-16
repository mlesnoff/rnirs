\name{dfplsrcov}
\alias{dfplsrcov}
\alias{dfplsrdiv}
\encoding{latin1}

\title{Degrees of freedom of PLSR1 Models}

\description{

Monte Carlo estimation of the model complexity \eqn{df} (number of degrees of freedom) of PLSR1 models. See in particular Ye 1998 and Efron 2004.

- In \code{dfplsrcov}, the covariances presented by Efron 2004 (Eq. 2.16) are calculated  by parametric bootstrap. The residual variance \eqn{sigma^2} is estimated from a low-biased model.

- In \code{dfplsrdiv}, the divergencies \eqn{dy_fit/dy} presented by Ye (1998) and Efron (2004) are calculated by perturbation analysis. This is a Stein unbiased risk estimation (SURE) of \eqn{df}.

}

\usage{

dfplsrcov(X, Y, ncomp, algo = NULL,
  B = 50,
  seed = NULL, print = TRUE, ...)

dfplsrdiv(X, Y, ncomp, algo = NULL, 
  ns = 50, meth.samp = c("syst", "random"), eps = 1e-4, 
  seed = NULL, print = TRUE, ...) 

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{Y}{A vector of length \eqn{n} of training responses. }

\item{ncomp}{The maximal number of PLS scores (= components = latent variables) to consider.}

\item{algo}{a PLS algorithm. Default to  \code{NULL} (\code{\link{pls.kernel}} is used).}

\item{B}{For \code{dfplsrcov}. Number of bootstrap replications.}

\item{ns}{For \code{dfplsrdiv}. Number of observations in the data receiving perturbation. The maximum is \eqn{n}.}

\item{meth.samp}{For \code{dfplsrdiv}. When \eqn{ns < n}, method used for sampling the observations receiving a perturbation. Possible values are "syst" (default; observations are sampled regularly over \eqn{y}) or "random".}

\item{eps}{For \code{dfplsrdiv}. The \eqn{epsilon} quantity used for scaling the perturbation analysis.}

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

## The example below reproduces the numerical illustration
## given by Kramer & Sugiyama 2011 on the Ozone data (Fig. 1, center).
## Note that function "pls.model" used for df calculations
## in the R package "plsdof" v0.2-9 (Kramer & Braun 2019)
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
u <- dfplsrcov(zX, y, ncomp = ncomp, B = B)
v <- dfplsrdiv(zX, y, ncomp = ncomp, ns = ns)

#library(plsdof)
#fm <- pls.model(
#  Xr, yr, m = ncomp, compute.DoF = TRUE,
#  compute.jacobian = FALSE,                   ## Krylov
#  use.kernel = FALSE
#  )
#df.kramer <- fm$DoF
df.kramer <- c(1.000000, 3.712373, 6.456417, 11.633565, 12.156760, 11.715101, 12.349716,
  12.192682, 13.000000, 13.000000, 13.000000, 13.000000, 13.000000)

zncomp <- 0:ncomp
plot(zncomp, u$df, type = "l", col = "red",
     ylim = c(0, 15),
     xlab = "Nb components", ylab = "df")
lines(zncomp, v$df, col = "blue")                 
lines(zncomp, zncomp + 1, col = "grey40")   ## Naive df
points(zncomp, df.kramer, pch = 16)
abline(h = 1, lty = 2, col = "grey")

}

\keyword{datagen}