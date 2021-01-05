\name{selcoef}
\alias{selcoef}
\encoding{latin1}

\title{Heuristic selection of the dimension of a PCA or PLS model using coefficients of bootstrapped loading vectors}

\description{

For PCA, Jackson (1993) presented the idea of using confidence intervals of boostrapped loading coefficients for selecting "meaningful" loading vectors, p.2208:

"the eigenvector coefficients were evaluated using a bootstrap approach. Coefficients that did not
differ significantly from zero were categorized as trivial or nonsignificant. However, if zero fell outside the 95pct confidence limits, then the coefficient was considered to be relatively stable and informative. Only bootstrapped components having two or more coefficients different from zero were considered meaningful. Components with only a single nonzero coefficient represented only a single variable, hence the component does not provide a true multivariate summary."

Unfortunately, Jackson 1993 did not detail how he computed the condition "if zero fell outside the 95pct confidence limits". This central point of the method stays finally unclear. Firstly, depending on the algorithm, the same loading vector can switch of sign. This also occurs for successive bootstrapped samples. A naive confidence interval on the bootstrapped coefficients can therefore contain "0" although the coefficient is in fact very meaningful. One solution is to consider the absolute value of the coefficients. But in such a case, "0" is the lower bound of the coefficients that is in practice never exactly reached by the calculated loading coefficients.

Face to these difficulties, function \code{selcoef} proposes the following approach.

A non parametric boostrap is implemented on the rows of matrix \eqn{X}. The loading matrix \eqn{P} is calculated for each replication. Then, the 95pct confidence interval is calculated (using the percentile method) for the absolute value of each coefficient of each loadinds vector (i.e. each column of \eqn{P}). A coefficinet is considered meaningful if the lower limit of its confidence interval is higher than a given critical value, set close to 0 (argument \code{lim}; in the function, the default is \code{lim = .01} which seems a good compromise). The function returns the proportion of non-meainingful coefficients for each dimension. Jumps in the curve, followed by regular patterns close to unity, are expected revealing unstable loadings built on noise.  

}

\usage{

selcoef(
    X, Y = NULL, ncomp = NULL, algo = NULL,
    B = 50, seed = NULL,
    alpha = .05, lim = .01,
    plot = TRUE, 
    xlab = "Nb. components", ylab = NULL,
    print = TRUE, 
    ...
    )

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of variables.}

\item{Y}{For PLS, a \eqn{n x q} matrix or data frame, or a vector of length \eqn{n}, of responses. If \code{NULL} (default) a PCA is implented.}

\item{ncomp}{The maximal number of PCA or PLS scores (= components = latent variables) to be calculated.}

\item{algo}{ For \code{pca}, a function (algorithm) implementing a PCA. Default to \code{NULL}: if \eqn{n < p}, \code{\link{pca_eigenk}} is used; in the other case, \code{\link{pca_eigen}} is used. For \code{pls}, a function implementing a PLS. Default to  \code{NULL} (\code{\link{pls_kernel}} is used).}

\item{B}{Number of bootstrap replications.}

\item{seed}{An integer defining the seed for the random simulation, or \code{NULL} (default). See \code{\link{set.seed}}.}

\item{alpha}{The confidence interval of the (absolute) coefficients is calculated with a level \eqn{100*(1 - alpha)pct}.} 

\item{lim}{The critical value for the lower limit of the confidence interval of the (absolute) coefficients.}

\item{plot}{Logical. If \code{TRUE} (default), results are plotted.}

\item{xlab}{Label for the x-axis of the plot.}

\item{ylab}{Label for the y-axis of the plot.}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in the function defined in \code{algo}.}

}

\value{

A list with output \code{p} = vector of the proportions of non-meaningful coefficients in the loading vectors.

}

\references{

Jackson, D.A., 1993. Stopping Rules in Principal Components Analysis: A Comparison of Heuristical and Statistical Approaches. Ecology 74, 2204-2214. https://doi.org/10.2307/1939574

}

\examples{

data(datcass)
Xr <- datcass$Xr
yr <- datcass$yr

ncomp <- 30
selcoef(Xr, yr, ncomp = ncomp, B = 30)

}

\keyword{datagen}