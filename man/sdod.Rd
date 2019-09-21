\name{sdod}
\alias{sdod}
\alias{sdodlist}
\encoding{latin1}

\title{Score and orthogonal distances (SD, OD)}

\description{

\code{sdod} is a wrapper for calculating score (SD) and orthogonal (OD) distances from a PCA or PLS(R/DA) model, using functions \code{\link{dis}} (with argument \code{diss = "Mahalanobis"}) and \code{\link{odis}}, respectively. See functions \code{\link{dis}} and \code{\link{odis}} for details.

\code{sdodlist} can be used for list of PCA or PLS(R/DA) models, for instance from outputs of \code{lwplsr} etc.

}

\usage{

sdod(fm, ncomp = NULL, ...)

sdodlist(fm, ncomp = NULL, ...)

}

\arguments{

\item{fm}{A PCA or PLS model (or a list of models for \code{sdodlist}). See examples.}

\item{ncomp}{The number of components (latent variables) to consider for calculating the distances. }

\item{...}{Optional arguments to pass in functions \code{\link{dis}} and \code{\link{odis}}.}

}


\examples{

data(datcass)

Xr <- datcass$Xr
Xu <- datcass$Xu

ncomp <- 15
fm <- pca(Xr, Xu, ncomp = ncomp)

a <- 10
res <- sdod(fm, ncomp = a)
names(res)

sdr <- res$sdr
odr <- res$odr
sdu <- res$sdu
odu <- res$odu

head(sdr)
head(odr)
head(sdu)
head(odu)

n <- 8
p <- 6
set.seed(1)
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
row.names(X) <- paste("l", 1:n, sep = "")
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)
set.seed(NULL)
Xr <- X[1:6, ] ; Yr <- Y[1:6, ]
Xu <- X[7:8, ] ; Yu <- Y[7:8, ]
Xr
Xu

ncompdis <- 3
k <- 5
ncomp <- 2
fm <- lwplsr(
  Xr, Yr,
  Xu, Yu,
  ncompdis = ncompdis,
  k = k,
  ncomp = ncomp,
  )
names(fm)
length(fm$fm)
names(fm$fm)

sdod(fm$fm[[1]])

sdodlist(fm$fm)$sdr
sdodlist(fm$fm)$odr
sdodlist(fm$fm)$sdu
sdodlist(fm$fm)$odu

}

\keyword{datagen}