\name{svmr}
\alias{svmr}
\alias{svmc}
\encoding{latin1}

\title{SVM Regression or Discrimination}

\description{

Function \code{svmr} and \code{svmc} are wrappers for fitting regression and discrimination SVM models, respectively, using function \code{\link{ksvm}} of package \code{kernlab} (thanks to Karatzoglou et al.) available on CRAN. This package uses the tool box LIVSIM (Chang & Lin, http://www.csie.ntu.edu.tw/~cjlin/libsvm). 

The functions use parameterization \eqn{'C'}, not the \eqn{'nu'} parameterization. 
 
The syntax and outputs are the same as for the other kernel models in  package \code{rnirs}.

See also the tuning facility with \code{\link{splitpar}}. Usual preliminary ranges are for instance:

- C <- 10^(-5:15)

- epsilon <- seq(.1, .3, by = .1)

- sigma <- 10^(-6:3)

}

\usage{

svmr(Xr, Yr, Xu, Yu = NULL, C = 1, epsilon = .1, kern = krbf, 
                 print = TRUE, ...)

svmc(Xr, Yr, Xu, Yu = NULL, C = 1, epsilon = .1, kern = krbf, 
                 print = TRUE, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n} of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A vector of length \eqn{m} of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{C}{A value, or vector of values, of the cost of constraints violation. This is the 'C'-constant of the regularization term in the Lagrange formulation}

\item{epsilon}{A value, or vector of values, of \eqn{epsilon} in the insensitive-loss function.}

\item{kern}{A function defining the considered kernel (Default to \code{\link{krbf}}). See \code{\link{krbf}} for syntax and other available kernel functions.}

\item{print}{Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern}. The value set in the kernel parameters (e.g. \code{sigma} for \code{\link{krbf}}) can be a scalar or a vector of several values.}

}

\value{

A list of outputs (see examples), such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\references{

Karatzoglou, A. et al. 2019. Package kernlab: Kernel-Based Machine Learning Lab. Version 0.9-29
https://cran.r-project.org/

Chang C., Lin C.. LIBSVM: a library for Support Vector Machines
http://www.csie.ntu.edu.tw/~cjlin/libsvm

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr
Xu <- datcass$Xu
yu <- datcass$yu
Xr <- detrend(Xr)
Xu <- detrend(Xu)
dim(Xr)
dim(Xu)

C <- 1e3
epsilon <- 0.1
sigma <- c(10, 100)
fm <- svmr(Xr, yr, Xu, yu, C = C, epsilon = epsilon, kern = krbf, sigma = sigma)
z <- mse(fm, ~ C + epsilon + sigma)
z[z$rmsep == min(z$rmsep), ][1, ]

data(datforages)
Xr <- datforages$Xr
yr <- datforages$yr
Xu <- datforages$Xu
yu <- datforages$yu
Xr <- savgol(snv(Xr), n = 21, p = 2, m = 2)
Xu <- savgol(snv(Xu), n = 21, p = 2, m = 2)
table(yr)
table(yu)

C <- 1e3
epsilon <- 0.1
sigma <- c(10, 100)
fm <- svmc(Xr, yr, Xu, yu, C = C, epsilon = epsilon, kern = krbf, sigma = sigma)
z <- err(fm, ~ C + epsilon + sigma)
z[z$errp == min(z$errp), ][1, ]

}

\keyword{datagen}