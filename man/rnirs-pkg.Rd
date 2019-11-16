\name{rnirs-pkg}
\alias{rnirs-pkg}
\docType{package}
\encoding{latin1}

\title{Locally Weighted PLS Regression and Discrimination}

\description{
Package rnirs is a statistical tool box focusing on locally weighted prediction methods: regression for quantitative responses and discrimination for qualitative responses. The predictive models can use partial least squares (PLS) dimension reductions or other methods. 

The package is primarily dedicated to near infrared spectral (NIRS) data but can be used for many other types of data.
}

\details{

\bold{------------ DATA CHECKING}
\tabular{ll}{
  \code{\link{checkdupl}}, \code{\link{rmdupl}} \tab Find or remove duplicated row observations between two data sets\cr
  \code{\link{checkna}} \tab Find and count NA values in a data set\cr
  }

\bold{------------ DATA SUMMARIZE}
\tabular{ll}{
  \code{\link{centr}} \tab Centers of classes\cr
  \code{\link{dtaggregate}} \tab Summary statistics with data subsets\cr
  \code{\link{summ}} \tab Summary of the variables of a data set\cr
  }

\bold{------------ DATA PREPROCESSING}
\tabular{ll}{
  \code{\link{dderiv}} \tab Derivation by finite difference\cr
  \code{\link{detrend}} \tab Detrend transformation\cr
  \code{\link{mavg}} \tab Smoothing by moving average \cr
  \code{\link{selw}} \tab Selection of wavelength ranges in spectra\cr
  \code{\link{savgol}} \tab Savitsky-Golay filtering\cr
  \code{\link{snv}} \tab Standard-normal-deviation transformation\cr
  }

\bold{------------ MULTIVARIATE FACTORIAL ANALYSES}
\tabular{ll}{
  \code{\link{fda}} \tab Factorial discriminant analysis\cr
  \code{\link{pca}} \tab PCA \cr
  \code{\link{pls}} \tab PLS \cr
  \code{\link{projscor}} \tab Projection on a score space \cr
  \code{\link{bcoef}} \tab b-coefficients for a PLS model\cr
  \code{\link{sdod}} \tab Score and orthogonal distances for a PCA or PLS score space\cr
  \code{\link{xfit}} \tab Matrix Approximation from multivariate scores and loadings \cr
  }

\bold{------------ VARIABLE SELECTION}
\tabular{ll}{
  \code{\link{covsel}} \tab CovSel\cr
  }

\bold{------------ GLOBAL PREDICTIVE METHODS}

\eqn{-- Regression}

\tabular{ll}{
  \code{\link{plsr}} \tab PLSR\cr
  \code{\link{lmr}} \tab LMR\cr
  }

\eqn{-- Discrimination}

\tabular{ll}{
  \code{\link{dalm}} \tab DA using multivariate linear regression\cr
  \code{\link{dadis}} \tab DA using the dissimilarity to class centers\cr
  \code{\link{daglm}} \tab DA using generalized linear models\cr
  \code{\link{dasdod}} \tab DA using a Simca index\cr
  \code{\link{daprob}} \tab Probablistic DA (parametric and non-parametric LDA and QDA)\cr
  \code{\link{plsda}}, \code{\link{plsdalm}} \tab PLSDA using various DA methods\cr
  }

\bold{------------ LOCALLY WEIGHTED PREDICTIVE METHODS}

\eqn{-- Regression}

\tabular{ll}{
  \code{\link{lwplsr}} \tab kNN-Locally weighted PLSR\cr
  \code{\link{knnwr}} \tab kNN-weighted regression \cr
  }

\eqn{-- Discrimination}

\tabular{ll}{
  \code{\link{lwplsda}}, \code{\link{lwplsdalm}} \tab kNN-Locally weighted PLSDA\cr
  \code{\link{knnwda}} \tab kNN-weighted discrimination \cr
  }

\bold{------------ MULTIBLOCK FUNCTIONS}

\tabular{ll}{
  \code{\link{orthog}} \tab Orthogonalization of a matrix to another matrix\cr
  \code{\link{blocksel}} \tab Block selection in a matrix\cr
  \code{\link{blockscal}} \tab Block scaling\cr
  \code{\link{blockpls}} \tab Block dimension reduction\cr
  }

\bold{------------ MODEL STACKING}
\tabular{ll}{
  \code{\link{stackmod}} \tab Model stacking for regression models\cr 
  }

\bold{------------ CROSS-VALIDATION AND ERROR RATES}
\tabular{ll}{
  \code{\link{fitcv}} \tab Cross-validation of a prediction model \cr
  \code{\link{mse}}, \code{\link{err}} \tab Error rates of prediction models\cr 
  \code{\link{segmcvkfold}}, \code{\link{segmcvmc}} \tab Segments for cross-validation \cr
  \code{\link{selncomp.wold}} \tab Selection of the number of components in a PCR/PLSR or PLSDA model \cr
  }

\bold{------------ GRAPHICS}
\tabular{ll}{
  \code{\link{plotmse}} \tab Plot error rates of prediction models\cr 
  \code{\link{plotsp}}, \code{\link{plotsp1}} \tab Plotting spectra (or more generally row observations) of a data set\cr
  \code{\link{plotxy}} \tab 2d scatter plot (e.g. for PCA or PLS scores or loadings)\cr
  }

\bold{------------ MISCELLANEOUS}
\tabular{ll}{
  \code{\link{kenston}} \tab Kennard-Stone sampling \cr
  \code{\link{matdis}} \tab Dissimilarity matrix (between observations)\cr
  \code{\link{sampclas}} \tab Within-class sampling\cr
  }

\bold{------------ ADVANCED FUNCTIONS}

\eqn{-- PCA and weighted PCA algorithms}

\tabular{ll}{
  \code{\link{pca.svd}}, \code{\link{pca.svdw}} \tab Svd algorithms \cr
  \code{\link{pca.eigen}}, \code{\link{pca.eigenw}} \tab Eigen algorithms \cr
  \code{\link{pca.nipals}} \tab Nipals algorithm with missing data allowed \cr
  }

\eqn{-- PLS and weighted PLS algorithms}

\tabular{ll}{
  \code{\link{pls.kernel}}, \code{\link{pls.kernelw}} \tab Kernel algorithms \cr
  \code{\link{pls.nipals}}, \code{\link{pls.nipalsw}} \tab Nipals algorithms \cr
  }

\eqn{-- Other}

\tabular{ll}{
  \code{\link{dis}}  \tab Dissimilarities between row observations and a given vector \cr
  \code{\link{dkern.gauss}}, \code{\link{dmnorm}} \tab Probability density prediction \cr
  \code{\link{dummy}} \tab Table of dummy variables \cr
  \code{\link{getknn}} \tab Knn selection \cr
  
  \code{\link{locw}} \tab Locally weighted models \cr
  \code{\link{matB}}, \code{\link{matW}} \tab Between and within covariance matrices \cr
  \code{\link{odis}} \tab Orthogonal distances from a PCA or PLS score space \cr
  
  \code{\link{pinv}} \tab Moore-Penrose pseudo-inverse \cr

  \code{\link{sourcedir}} \tab Source R functions in a directory \cr
  \code{\link{wkern}} \tab Kernel weights \cr
  }

}

\author{
  Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>\cr
  Maintainer: Matthieu Lesnoff <matthieu.lesnoff@cirad.fr>
  }
  
\keyword{package}
