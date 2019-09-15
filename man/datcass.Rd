\name{datcass}
\alias{datcass}
\docType{data}
\encoding{latin1}

\title{datcass}

\description{
A NIRS data set describing the concentration of a natural pigment in samples of tropical shrubs. Spectra were recorded from 400 to 2498 nm at 2 nm intervals.
}

\usage{data(datcass)}

\format{
  A list with 4 components: \code{Xr}, \code{yr}, \code{Xu}, \code{yu}.
  
  \describe{
  
----- Reference (calibration) data

    \item{Xr}{ A matrix whose rows are the NIR absorbance spectra (= log10(1 / Reflectance)).}

    \item{yr}{A vector of the response variable (pigment concentration).}
    
----- Test data

    \item{Xu}{A matrix whose rows are the NIR absorbance spectra (= log10(1 / Reflectance)).}

    \item{yu}{A vector of the response variable (pigment concentration).}
    
  }
}

\references{

Davrieux, F., Dufour, D., Dardenne, P., Belalcazar, J., Pizarro, M., Luna, J., Londono, L., Jaramillo, A., Sanchez, T., Morante, N., Calle, F., Becerra Lopez-Lavalle, L., Ceballos, H., 2016. LOCAL regression algorithm improves near infrared spectroscopy predictions when the target constituent evolves in breeding populations. Journal of Near Infrared Spectroscopy 24, 109. https://doi.org/10.1255/jnirs.1213

CIAT Cassava Project (Colombia), CIRAD Qualisud Research Unit, and funded mainly by the CGIAR Research Program on Roots, Tubers and Bananas (RTB) with support from CGIAR Trust Fund contributors (https://www.cgiar.org/funders/).

}

\examples{

data(datcass)
str(datcass)

}


\keyword{datasets}
