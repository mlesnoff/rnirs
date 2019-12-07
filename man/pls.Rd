\name{pls}
\alias{pls}
\alias{pca}
\encoding{latin1}

\title{PCA or PLS}

\description{

Fitting PCA, PLS1 or PLS2.

\code{pca} and \code{pls} use the denominator \eqn{n - 1} (not \eqn{n}) in the variance, covariance and correlation calculations.

}

\usage{
pca(Xr, Xu = NULL, ncomp, algo = pca.svd, ...)

pls(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{The number of PCA or PLS scores (i.e. components) to be calculated.}

\item{algo}{A function (algorithm) implementing a PCA (default to \code{\link{pca.svd}}) or a PLS (default to \code{\link{pls.kernel}}).}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list of outputs (see examples), such as:

\item{Tr}{The Xr-score matrix (\eqn{n x ncomp}).}

\item{Tu}{The Xu-score matrix (\eqn{m x ncomp}).}

\item{P}{The Xr-loadings matrix (\eqn{p x ncomp}).}

\item{explvar}{Proportions of \eqn{Xr-}variance explained by the Xr-score space.}

And other outputs: See the examples and function defined in \code{algo}.

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

##################### PCA

ncomp <- 3
fm <- pca(Xr, ncomp = ncomp)
names(fm)
fm$Tr   
fm$P

### Proportion of explained X-variance

z <- fm$explvarx
z
barplot(100 * z$pvar, names.arg = paste("comp", z$ncomp), 
  ylab = "Pct. of variance explained")

### Scores

comp <- c(1, 2)
#comp <- c(2, 3)
plotxy(fm$Tr[, comp])

plotxy(fm$Tr[, comp], label = TRUE)

### Contributions of the individuals (in proportions)

fm$contr.ind
colSums(fm$contr.ind)

### Loadings

plotxy(fm$P, label = TRUE, origin = c(0, 0))

### Coordonates of the variables

plotxy(fm$coord.var, label = TRUE, origin = c(0, 0))

### Contributions of the variables (in proportions)

fm$contr.var
colSums(fm$contr.var)

### Correlation circle

fm$cor.circle

comp <- c(1, 2)
#comp <- c(2, 3)
z <- fm$cor.circle[, comp]
plotxy(z, label = TRUE, origin = c(0, 0), circle = TRUE)

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pca(Xr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

res <- rbind(fm$Tr, fm$Tu)
group <- c(rep("Reference", nrow(fm$Tr)), 
  rep("Unknown", nrow(fm$Tu)))
comp <- c(1, 2)
#comp <- c(2, 3)
plotxy(res[, comp], group = group, label = TRUE, origin = c(0, 0)) 

### Syntax with weighting

pca(Xr, Xu, ncomp = ncomp, algo = pca.svdw, 
  weights = rep(1, nrow(Xr)))

##################### PLS

ncomp <- 3
fm <- pls(Xr, Yr, ncomp = ncomp)
names(fm)
fm$Tr
fm$P

### Proportion of explained X-variance

fm$explvarx

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pls(Xr, Yr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

### Syntax with weighting

pls(Xr, Yr, Xu, ncomp = ncomp, algo = pls.kernelw, 
  weights = rep(1, nrow(Xr)))$Tu

}









\keyword{datagen}