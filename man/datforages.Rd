\name{datforages}
\alias{datforages}
\docType{data}
\encoding{latin1}

\title{datforages}

\description{
A NIRS pre-processed data set describing the class membership of forages. Spectra were recorded from 1100 to 2498 nm at 2 nm intervals.
}

\usage{data(datforages)}

\format{
  A list with 4 components: \code{Xr}, \code{yr}, \code{Xu}, \code{yu}.
  
  \describe{
  
----- Reference (calibration) data

    \item{Xr}{A matrix whose rows are the pre-processed NIR absorbance spectra (= log10(1 / Reflectance)).}

    \item{yr}{A vector of the response variable (class membership).}
    
----- Test data

    \item{Xu}{A matrix whose rows are the pre-processed NIR absorbance spectra (= log10(1 / Reflectance)).}

    \item{yu}{A vector of the response variable (class membership).}
    
  }
}

\examples{

data(datforages)
str(datforages)

}


\keyword{datasets}
