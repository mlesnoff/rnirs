\name{cvfit}
\alias{cvfit}
\encoding{latin1}

\title{Cross-validation of a prediction model}

\description{

\code{cvfit} is a generic function for implementing cross-validation of a prediction model. Argument \code{fun} defines the function implementing the model has to be cross-validated.

}

\usage{

cvfit(X, Y, fun, segm, printcv = FALSE, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of reference (= training) row observations.}

\item{Y}{For quantive responses: A \eqn{n x q} matrix or data frame, or vector of length \eqn{n}, of reference (= training) responses. For qualitative responses: A vector of length \eqn{n}, or \eqn{n x 1} matrix,  of reference (= training) responses (class membership).}

\item{fun}{A function defining the prediction model to cross-validate.}

\item{segm}{A list of the test segments. Typically, output of function \code{\link{segmkf}} or \code{\link{segmts}}.} 

\item{printcv}{Logical. If \code{TRUE} (default), fitting information are printed.}

\item{...}{Optionnal arguments to pass through function \code{fun}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

}


\examples{

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

Xr <- detrend(Xr)
headm(Xr)

################ Cross-validation of a PLSR model

n <- nrow(Xr)
segm <- segmkf(n = n, K = 5, typ = "random", nrep = 3)      # = Repeated K-fold CV
#segm <- segmts(n = n, m = 40, nrep = 30)                   # = Repeated Test-set CV
fm <- cvfit(
  Xr, yr,
  fun = plsr,
  ncomp = 20,
  segm = segm,
  printcv = TRUE
  )
names(fm)
headm(fm$y)
headm(fm$fit)
headm(fm$r)

z <- mse(fm, ~ ncomp)
headm(z)
z[z$msep == min(z$msep), ]
plotmse(z)
plotmse(z, nam = "r2", ylim = c(0, 1))
abline(h = 1, lty = 2, col = "grey")

z <- mse(fm, ~ ncomp + rep)
headm(z)
plotmse(z, group = z$rep)
plotmse(z, group = z$rep, legend = FALSE, col = "grey")

################ Example of CV of an ad'hoc predictive function (e.g. using locw)

fun <- function(Xr, Yr, Xu, Yu = NULL, k, ncomp, ncompdis) {
  
  z <- pls(Xr, Yr, Xu, ncomp = ncompdis)
  resn <- getknn(z$Tr, z$Tu, k = k, diss = "mahalanobis")
  fm <- locw(
    Xr, Yr,
    Xu, Yu,
    listnn = resn$listnn,
    listw = lapply(resn$listd, wdist, h = 2),
    fun = plsr,
    algo = pls_kernel,
    ncomp = ncomp,
    print = TRUE
    )

  list(y = fm$y, fit = fm$fit, r = fm$r)
  
  }

n <- nrow(Xr)
segm <- segmkf(n = n, K = 5, typ = "random", nrep = 1)
fm <- cvfit(
  Xr, yr,
  fun = fun,
  k = 50,
  ncomp = 20,
  ncompdis = 10,
  segm = segm,
  printcv = TRUE
  )
headm(fm$fit)

z <- mse(fm, ~ k + ncomp)
headm(z)
z[z$msep == min(z$msep), ]
plotmse(z, group = z$k)

}

\keyword{datagen}