\name{sdod}
\alias{sdod}
\encoding{latin1}

\title{Score and orthogonal distances (SD, OD)}

\description{

\code{sdod} is a wrapper for calculating score (SD) and orthogonal (OD) distances from a PCA or PLS model, using functions \code{\link{dis}} (with argument \code{diss = "Mahalanobis"}) and \code{\link{odis}}, respectively. See functions \code{\link{dis}} and \code{\link{odis}} for details.

}

\usage{

sdod(Xr, Xu, fm, ncomp = NULL, ...)

}

\arguments{

\item{Xr}{The matrix or data frame of reference (= training) observations that was used for building the preliminary model \code{fm}.}

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations (\eqn{Xu} is not used in the calculation of the cutoff that defines the extreme distance values).}

\item{fm}{Output of one of the functions implementing the PCA or PLS.}

\item{ncomp}{The number of PCA or PLS components (i.e. scores) to consider. If \code{NULL} (default), the maximal number of components computed in the PCA or PLS is considered.}

\item{...}{Optional arguments to pass in functions \code{\link{dis}} and \code{\link{odis}}.}

}


\examples{

data(datcass)

############# REFERENCE DATA SET

Xr <- datcass$Xr
Xu <- datcass$Xu

ncomp <- 15
fm <- pca(Xr, ncomp = 15)

## Computations in a score space of 'a' components

a <- 10
res <- sdod(Xr, fm = fm, ncomp = a)
names(res)

sdr <- res$sdr
odr <- res$odr
head(sdr)
head(odr)

## mean of the squared SD in the reference data set (should be ~ a)
mean(sdr$d^2)

## extreme SD values 
z <- sdr
which(z$dstand > 1) 
cut <- z$d[1] / z$dstand[1]
oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plot(ecdf(z$d), xlab = "OD", pch = 1, ylab = "Fn(OD)", main = "")
abline(v = cut, col = "red", lty = 4)
hist(z$d, n = 100, col = "grey", xlab = "OD", main = "")
abline(v = cut, col = "red", lty = 4)
par(oldpar)

## extreme OD values 
z <- odr
which(z$dstand > 1) 
cut <- z$d[1] / z$dstand[1]
oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plot(ecdf(z$d), xlab = "OD", pch = 1, ylab = "Fn(OD)", main = "")
abline(v = cut, col = "red", lty = 4)
hist(z$d, n = 100, col = "grey", xlab = "OD", main = "")
abline(v = cut, col = "red", lty = 4)
par(oldpar)

## SD-OD plot
## (observations in the right-up side of the plot have "bad" leverages)

cut.sd <- sdr$d[1] / sdr$dstand[1]
cut.od <- odr$d[1] / odr$dstand[1]

plot(sdr$d, odr$d, xlab = "SD", ylab = "OD")
abline(v = cut.sd, col = "red", lty = 4)
abline(h = cut.od, col = "red", lty = 4)

## Plotting spectra Xr after ordering the observations by the OD distance
## (spectra having close OD will show a similar color)
## Below data are sorted by decreasing distance to plot the farest spectra first:

z <- sdr[order(sdr$d, decreasing = TRUE), ]
head(z)
plotsp(X = Xr[z$rownum, ], col.low = "grey10", col.high = "grey90")
# add the average spectrum
v <- centr(Xr, FUN = mean)$centers 
plotsp(X = v, col = "red", add = TRUE, lwd = 2)

############# REFERENCE vs. TEST DATA SETS

ncomp <- 15
fm <- pca(Xr, ncomp = ncomp)

a <- 10
res <- sdod(Xr, Xu, fm, ncomp = a)
names(res)

sdr <- res$sdr
odr <- res$odr
sdu <- res$sdu
odu <- res$odu

head(sdr)
head(odr)
head(sdu)
head(odu)

cut.sd <- sdr$d[1] / sdr$dstand[1]
cut.od <- odr$d[1] / odr$dstand[1]

plot(sdr$d, odr$d, xlab = "SD", ylab = "OD", 
  xlim = c(0, max(sdr$d, sdu$d)), ylim = c(0, max(odr$d, odu$d)))
points(sdu$d, odu$d, col = "purple", pch = 16)
abline(v = cut.sd, col = "red", lty = 4)
abline(h = cut.od, col = "red", lty = 4)

}

\keyword{datagen}