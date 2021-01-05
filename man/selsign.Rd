\name{selsign}
\alias{selsign}
\encoding{latin1}

\title{Heuristic selection of the dimension of regression models with permutation tests}

\description{

The function helps selecting regression models using non parametric permutation tests, in particular for finding relevent dimensions (i.e. nb. components) of PCR, PLSR, ... models.

The principle is to compare the central location of the distributions of squared residuals returned by a cross-validation (PRESS), using permutation tests for matched pairs.

A CV is firstly implemented. Then, the model \eqn{M0} with miminal MSEP is found. The PRESS distribution of the other models are finally compared to the PRESS distribution of \eqn{M0}. For a given observation, the squared residual from \eqn{M0} is matched with the squared residuals returned by the other models

Depending on argument \code{type}, three matched-pairs tests are proposed 

- \code{"sign"}: Sign-test (Convover 1999), using function \code{\link{binom.test}}.

- \code{"wilcox"}: Wilcoxon signed-rank test (Convover 1999), using function \code{\link{wilcox.test}}.

- \code{"perm"}: Randomization permutation test. At our knowledge, this test was firstly applied to chemometrics by van der Voet 1994. Using Monte Carlo randomization, it is more time-consuming than the two previous tests. 

The function returns the p-value of the on-side test \eqn{H0: PRESS(M0) = PRESS(M)} vs \eqn{H1: PRESS(M0) < PRESS(M)}. 

}

\usage{

selsign(
    fm, formula = ~ 1, 
    nam = NULL,
    type = c("sign", "wilcox", "perm"), 
    nperm = 50, seed = NULL
    )

}

\arguments{

\item{fm}{An object returned by a CV process, basically the output of function \code{\link{cvfit}}.}

\item{formula}{A right-hand-side formula defing the aggregation levels (i.e. the different compared models).}

\item{nam}{Name (character string) of the column to consider in \code{fm$y} for calulating the PRESS. If \code{NULL} (default), the last column of \code{fm$y} is considered.}

\item{type}{Type of test. Possible values are \code{"sign"} (default), \code{"wilcox"} and \code{"perm"}.}

\item{nperm}{Number of random permutations if \code{"test = perm"}.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}
}

\value{

A list with outputs, see the examples.

}

\references{

Conover, W.J., 1999. Practical nonparametric statistics, 3rd ed. ed, Wiley series in probability and statistics. Applied probability and statistics section. Wiley, New York.

van der Voet, H., 1994. Comparing the predictive accuracy of models using a simple randomization test. Chemometrics and Intelligent Laboratory Systems 25, 313-323. https://doi.org/10.1016/0169-7439(94)85050-X

- https://en.wikipedia.org/wiki/Sign_test

- https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr
n <- nrow(Xr)

segm <- segmkf(n = n, K = 5, nrep = 2)  
ncomp <- 20
fm <- cvfit(
  Xr, yr,
  fun = plsr,
  ncomp = ncomp,
  segm = segm,
  print = TRUE
  )
z <- mse(fm, ~ ncomp)
plotmse(z)

z <- selsign(fm, ~ ncomp, type = "wilcox")
names(z)
headm(z$pval)
zncomp <- seq(0, ncomp)
pval <- z$pval$pval
plot(zncomp, pval, type = "b", pch = 16, col = "#045a8d",
     xlab = "Nb components", ylab = "p-value")
alpha <- .10
abline(h = alpha, col = "grey")
u <- which(pval >= alpha)
opt <- min(u) - 2
opt

######## More complex models

segm <- segmkf(n = n, K = 2, nrep = 2)  
ncomp <- 5
fm <- cvfit(
  Xr, yr,
  fun = dkplsr,
  ncomp = ncomp,
  degree = c(1, 2),
  segm = segm,
  print = TRUE
  )
head(fm$y)

z <- selsign(fm, ~ ncomp + degree)  
z

}

\keyword{datagen}