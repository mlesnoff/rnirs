\name{plsr}
\alias{plsr}
\encoding{latin1}

\title{PLSR models}

\description{

Function \code{plsr} fits PLSR1 or PLSR2 models, using function \code{\link{pls}}.

}

\usage{

plsr(Xr, Yr, Xu, Yu = NULL, ncomp, algo = pls.kernel, 
  stor = FALSE, ...)

}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to predict.}

\item{Yu}{A \eqn{m x q} matrix or data frame, or a vector of length \eqn{m}, of the true responses for \eqn{Xu}. Default to \code{NULL}.}

\item{ncomp}{The number of PLS scores (i.e. components) to consider. .}

\item{algo}{A function (algorithm) implementing a PLS. Default to \code{\link{pls.kernel}}.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

\item{stor}{Logical (default to \code{FALSE}). If \code{TRUE}, the function stores all the outputs of function \code{\link{pls}} and  auxilliary statistics  (e.g. b-coefficients, and score and orthogonal distances for the new observations to predict), in a sub-object \code{fm}.}

}

\value{

A list of outputs, such as:

\item{y}{Responses for the test data.}

\item{fit}{Predictions for the test data.}

\item{r}{Residuals for the test data.}

\item{fm}{Outputs of \code{\link{pls}} and other auxilliary statistics. \code{NULL} if \code{stor = FALSE}.}

}

\examples{

n <- 10
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)

Xr <- X[1:8, ] ; Yr <- Y[1:8, ] 
Xu <- X[9:10, ] ; Yu <- Y[9:10, ] 

ncomp <- 3
plsr(Xr, Yr, Xu, Yu, ncomp = ncomp)
plsr(Xr, Yr, Xu, ncomp = ncomp)
plsr(Xr, Yr[, 1], Xu, Yu[, 1], ncomp = ncomp)

## With weights

ncomp <- 3
plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, 
  algo = pls.kernelw, weights = rep(1, nrow(Xr)))

## mse and plotmse

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp)
mse(fm, ~ ncomp, nam = "y1")
mse(fm, ~ ncomp, nam = "y2")
mse(fm, ~ ncomp)

z <- mse(fm, ~ ncomp)
z
z[z$rmsep == min(z$rmsep), ]
plotmse(z)

## Argument "stor" stores all the outputs of function pls
## and auxilliary statistics for the specific model with ncomp
## components (e.g. b-coefficients, and score and orthogonal
## distances), in a sub-object "fm"

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, stor = TRUE)
names(fm)
fm$fm
# for the specific model with ncomp components
fm$fm$bcoef   # b-coefficients
fm$fm$sd      # score distances SD
fm$fm$od      # orthogonal distances OD

fm$fit[fm$fit$ncomp == ncomp, ]
cbind(rep(1, nrow(Xu)), Xu) %*% fm$fm$bcoef

## b coefficients with function bcoef

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, stor = TRUE)
fm$fm$bcoef
bcoef(fm$fm)

# bcoef can set any number of components
bcoef(fm$fm, ncomp = 2)

## SD and OD with function sdod

ncomp <- 3
fm <- plsr(Xr, Yr, Xu, Yu, ncomp = ncomp, stor = TRUE)
fm$fm$sd
fm$fm$od
sdod(Xr, Xu, fm$fm)$sdu
sdod(Xr, Xu, fm$fm)$odu

# sdod can set any number of components
sdod(Xr, Xu, fm$fm, ncomp = 2)

}

\keyword{datagen}