\name{darr}
\alias{darr}
\alias{dakrr}
\encoding{latin1}

\title{DA using Ridge or Kernel Ridge Regression on the Y-Dummy table}

\description{

DA-RR or DA-KRR

1- The class membership \eqn{y} (unidimensional variable) for the reference (= training) observations is firstly transformed (with function \code{\link{dummy}}) to a table \eqn{Ydummy} containing a number of \eqn{nclas} dummy variables, where \eqn{nclas} is the number of classes in \eqn{y}. 

2- Then, a linear ridge regression (RR) or a kernel ridge regression (KRR = LS-SVM) model is fitted between the \eqn{X}-data and each of the dummy variables (i.e. each column of the dummy table \eqn{Ydummy}). 

3- For a given new observation, the final prediction (a class) corresponds to the dummy variable for which the prediction is the highest.

When the number of classes is higher than two, this method  can be affected by a masking effect (see eg. Hastie et al. 2009, section 4.2): some class(es) can be masked (therefore not well predicted) if more than two classes are aligned in the \eqn{X}-space. Caution should therefore be taken about such eventual masking effects.

Row observations can eventually be weighted with a priori weights (using argument \code{weights}).

\bold{Note:} An alternative to \code{dakrr} is to run a "direct" kernel approach (e.g. Bennett & Embrechts 2003), i.e. to build preliminary kernel Gram matrices (such as doing a pre-processing on \eqn{X}), and then to run a DA-RR on them. See examples in function \code{\link{kgram}}.

See also the tuning facility with \code{\link{splitpar}}.

}

\usage{

darr(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, 
                 weights = NULL)
                 
dakrr(Xr, Yr, Xu, Yu = NULL, lambda = 0, unit = 1, 
                 kern = kpol, weights = NULL, print = TRUE, ...)                 
                 
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A vector of length \eqn{n}, or a \eqn{n x 1} matrix, of reference (= training) responses (class membership).}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be predicted.}

\item{Yu}{A vector of length \eqn{m}, or a \eqn{m x 1} matrix, of the true response (class membership). Default to \code{NULL}.}

\item{lambda}{A scalar or numeric vector (giving several values of \eqn{lambda}) defining the regularization parameter value(s).}

\item{unit}{Unit used for lambda (Default to \code{unit = 1}). For instance, \code{lambda = 12, unit = 1e-6, ...} means that \code{lambda = 12e-6}.}

\item{kern}{For \code{dakrr}. A function defining the considered kernel (Default to \code{\link{kpol}}). See \code{\link{kpol}} for syntax and other available kernel functions.}

\item{weights}{A vector of length \eqn{n} defining a priori weights to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{print}{For \code{dakrr}. Logical (default = \code{TRUE}). If \code{TRUE}, fitting information are printed.}

\item{...}{For \code{dakrr}. Optionnal arguments to pass in the kernel function defined in \code{kern}. The value set in the kernel parameters (e.g. \code{degree} for \code{\link{kpol}}) can be a vector of several parameter values.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}

\examples{

data(datforages)

Xr <- datforages$Xr
yr <- datforages$yr

Xu <- datforages$Xu
yu <- datforages$yu

lambda <- 10^(-15:5) ; unit <- 1
fm <- darr(Xr, yr, Xu, yu, lambda = lambda)
z <- err(fm, ~ lambda + unit)
z[z$errp == min(z$errp), ][1, ]
z$log.lambda <- log(z$lambda, base = 10)
plotmse(z, namx = "log.lambda", nam = "errp")

lambda <- 10^(-15:5) ; unit <- 1
fm <- dakrr(Xr, yr, Xu, yu, lambda = lambda, degree = 1:5)
z <- err(fm, ~ lambda + unit + degree)
z[z$errp == min(z$errp), ][1, ]
z$log.lambda <- log(z$lambda, base = 10)
group <- paste("degree=", z$degree, sep = "")
plotmse(z, namx = "log.lambda", nam = "errp", group = group)

lambda <- 10^(-15:5) ; unit <- 1
fm <- dakrr(Xr, yr, Xu, yu, lambda = lambda, kern = krbf, sigma = 10^(-3:3))
z <- err(fm, ~ lambda + unit + sigma)
z[z$errp == min(z$errp), ][1, ]
z$log.lambda <- log(z$lambda, base = 10)
group <- paste("sigma=", z$sigma, sep = "")
plotmse(z, namx = "log.lambda", nam = "errp", group = group)

}

\keyword{datagen}