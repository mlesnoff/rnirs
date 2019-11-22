## rnirs 1.2-2
## Locally Weighted PLS Regression and Discrimination

R package focusing on locally weighted prediction methods

- Regression for quantitative responses
- Discrimination for qualitative responses 

The prediction models can use partial least squares dimension reductions (PLS) or other methods.

The package can handle many types of data, although it was primarily dedicated to near infrared spectral (NIRS) data.

The changes in the successive versions of the package are reported in the **NEWS** file above.

Using **Rstudio** is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

### Main features

* Data checking and summarizing
* Data pre-processing: detrend, SNV, derivations
* Multivariate factorial analyses: PCA, PLS, FDA
* Variable selection: CovSel
* Global predictive methods
    - Quantitative regressions: PLSR, LMR
    - Discrimination: PLSDA with various DA methods
* Locally weighted predictive methods
    - Quantitative regressions: kNN-LWPLSR, kNN weighted regressions
    - Discrimination: kNN-LWPLSDA with various DA methods, kNN weighted discrimination
    - Generic function **locw** for implementing any locally weighted models
* Multiblock functions:
    - Orthogonalization of a matrix to another matrix
    - block-selection
    - block-scaling
    - block-dimension reduction by PLS or PCA
    - block-dimension reduction by SO-PLS or SO-PCA
* Cross-validation
    - Generic function **fitcv**
* Graphics
* Miscellaneous functions

### Installation from Github

1. If not already done, install **devtools** package from the CRAN

```{r}
install.packages("devtools")
```

2. Load **devtools** and then install **rnirs** package

```{r}
library(devtools)
install_github("mlesnoff/rnirs", dependencies = TRUE)
```

### Installation from the Tar file rnirs_....tar.gz with Rstudio

1. Install packages from the CRAN (Menu "Packages", "Install", and then "Install From CRAN")

- **scales**
- **signal**
- **data.table**
- **FNN**
- **ggplot2**
- **gridExtra** 

2. Install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")

### Usage

Load **rnirs**

```{r}
library(rnirs)
```
To get an overview of the available functions

```{r}
??rnirs
```
### Contact

Matthieu Lesnoff
matthieu.lesnoff@cirad.fr

