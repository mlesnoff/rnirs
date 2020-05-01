\name{pls}
\alias{pls}
\alias{pca}
\alias{influ.pls}
\encoding{latin1}

\title{PCA or PLS}

\description{

Functions \code{pca} and \code{pls} fit PCA and PLS1 or PLS2, respectively.

Functions \code{influ.pls} calculates regression influence measures (e.g. Cook's distance) of the observations, conditinonnally to the \eqn{ncomp} scores computed in the PLS.

}

\usage{
pca(Xr, Xu = NULL, ncomp, algo = pca.svd, ...)

pls(Xr, Yr, Xu = NULL, ncomp, algo = pls.kernel, ...)

influ.pls(fm, ncomp = NULL)
}

\arguments{

\item{Xr}{A \eqn{n x p} matrix or data frame of reference (= training) observations.}

\item{Yr}{A \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of reference (= training) responses. }

\item{Xu}{A \eqn{m x p} matrix or data frame of new (= test) observations to be projected in the calculated reference score space (\eqn{Xu} is not used in the calculation of this score space). Default to \code{NULL}.}

\item{ncomp}{In \code{pca} and \code{pls}: The number of PCA or PLS scores (i.e. components) to be calculated. In \code{influ.pls}: The number of scores considered in the calculation of the regression infleunce measures. If \code{NULL} (default), this is the total number of scores of the PLS model.}

\item{algo}{A function (algorithm) implementing a PCA (default to \code{\link{pca.svd}}) or a PLS (default to \code{\link{pls.kernel}}).}

\item{fm}{In \code{influ.pls}: The PLS model, output of functions \code{pls}, \code{\link{plsr}}) or \code{\link{plsda}}).}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

- For \code{pca} and \code{pls}:

A list of outputs (see examples), such as:

\item{Tr}{The Xr-score matrix (\eqn{n x ncomp}).}

\item{Tu}{The Xu-score matrix (\eqn{m x ncomp}).}

\item{P}{The Xr-loadings matrix (\eqn{p x ncomp}).}

\item{explvar}{Proportions of \eqn{Xr-}variance explained by the Xr-score space.}

And other outputs: See the examples and function defined in \code{algo}.

- For \code{influ.pls}:

\item{r}{The raw (i.e. crude) residuals.}

\item{rstud.int}{The internally studentized residuals.}

\item{r.press}{The PRESS (i.e. deleted) residuals.}

\item{cook}{The Cook's distances.}

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
plotxy(fm$Tr[, comp], zeroes = TRUE)

plotxy(fm$Tr[, comp], zeroes = TRUE, labels = TRUE)

### Contributions of the individuals (in proportions)

fm$contr.ind
colSums(fm$contr.ind)

### Loadings

plotxy(fm$P, zeroes = TRUE, label = TRUE)

### Coordonates of the variables

plotxy(fm$coord.var, zeroes = TRUE, label = TRUE)

### Contributions of the variables (in proportions)

fm$contr.var
colSums(fm$contr.var)

### Correlation circle

fm$cor.circle

plotxy(fm$cor.circle, zeroes = TRUE, labels = TRUE, circle = TRUE, ylim = c(-1, 1))

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pca(Xr, Xu, ncomp = ncomp)
Tr <- fm$Tr
Tu <- fm$Tu

T <- rbind(Tr, Tu)
row.names(T) <- 1:nrow(T)
group <- c(rep("Reference", nrow(Tr)), rep("Unknown", nrow(Tu)))
plotxy(T, group = group, pch = 16, zeroes = TRUE)
plotxy(T, group = group, labels = TRUE, zeroes = TRUE)


##################### PLS

ncomp <- 3
fm <- pls(Xr, Yr, ncomp = ncomp)
names(fm)
fm$Tr
fm$P

### Proportion of explained X-variance

fm$explvarx

### Influence regression measure

influ.pls(fm)

### Projection of Xu on the score space Tr ==> Scores Tu

fm <- pls(Xr, Yr, Xu, ncomp = ncomp)
fm$Tr
fm$Tu

}

\keyword{datagen}