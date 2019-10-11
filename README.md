## rnirs 1.1-0
## Locally Weighted PLS Regression and Discrimination

R package focusing on locally weighted prediction methods

- Regression for quantitative responses
- Discrimination for qualitative responses 

The prediction models can use partial least squares dimension reductions (PLS) or other methods.

The package is primarily dedicated to near infrared spectral data (NIRS) but is generic for many other types of data.

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

Changes related to the package versions are reported in the NEWS file.

### Main features

* Data checking and summarizing
* Data pre-processing (detrend, SNV, derivations)
* Multivariate factorial analyses: PCA, PLS, FDA
* Global predictive methods
    - Quantitative regressions: PLSR
    - Discrimination: PLSDA with various DA methods
* Locally weighted predictive methods
    - Quantitative regressions: kNN-LWPLSR, kNN weighted regressions
    - Discrimination: kNN-LWPLSDA with various DA methods, kNN weighted discrimination
    - The generic function **locw** can implement any locally weighted models
* Cross-validation
    - Generic function **fitcv**
* Graphics
* Other functions

### Installation from Github

1. Install **devtools** package from the CRAN (if not already done)

```{r}
install.packages(devtools)
```

2. Load **devtools** and then install **rnirs** package

```{r}
library(devtools)
install_github("mlesnoff/rnirs", dependencies = TRUE)
```

### Usage

Load **rnirs**

```{r}
library(rnirs)
```
To get an overview of the available functions

```{r}
??rnirs
```


