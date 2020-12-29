\name{splitpar}
\alias{splitpar}
\encoding{latin1}

\title{Split a parameter value within an interval}

\description{

Function \code{splitpar} split the value of a model parameter within a given range (interval). The function is a help for model tuning (e.g. RR, KRR, SVM etc.).  
}

\usage{

splitpar(x, a1 = .1, a2 = 10, n = 20)

}

\arguments{

\item{x}{A scalar.}

\item{a1}{The lower limit of the range interval is \eqn{a1 * x}.}

\item{a2}{The upper limit of the range interval is \eqn{a2 * x}.}

\item{n}{Approximate number of output values within the range.}

}

\value{

A vector (grid of parameter values).

}

\examples{

lambda <- 1e-2
z <- splitpar(lambda, .1, 10, n = 20)
z
plot(z)

################## EXAMPLE OF TUNING

data(datcass)

Xr <- datcass$Xr
yr <- datcass$yr

Xu <- datcass$Xu
yu <- datcass$yu

Xr <- detrend(Xr)
Xu <- detrend(Xu)
dim(Xr)
dim(Xu)

### First round

lambda <- 10^(-15:5) ; unit <- 1
sigma <- 10^(-6:3)
fm <- krr(Xr, yr, Xu, yu, lambda = lambda, unit = unit, kern = krbf, sigma = sigma)
z <- mse(fm, ~ lambda + unit + sigma)
u <- z[z$rmsep == min(z$rmsep), ][1, ]
u
z$log.lambda <- log(z$lambda, base = 10)
group <- paste("sigma=", z$sigma, sep = "")
plotmse(z, namx = "log.lambda", group = group)

### Second round

lambda <- splitpar(u$lambda)
sigma <- splitpar(u$sigma)
fm <- krr(Xr, yr, Xu, yu, lambda = lambda, unit = unit, kern = krbf, sigma = sigma)
z <- mse(fm, ~ lambda + unit + sigma)
u <- z[z$rmsep == min(z$rmsep), ][1, ]
u

### Final estimate

fm <- krr(Xr, yr, Xu, yu, lambda = u$lambda, unit = u$unit, kern = krbf, sigma = u$sigma)
fit <- fm$fit$y1
r <- yu - fit
sqrt(mean(r^2))
plot(yu, fit)
abline(0, 1, col = "grey")


}

\keyword{datagen}