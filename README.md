# R package rnirs 1.0-9  
### Locally Weighted PLS Regression and Discrimination

Package **rnirs** is a statistical tool box focusing on locally weighted prediction methods: regression for quantitative responses and discrimination for qualitative responses. The predictive models can use partial least squares (PLS) dimension reductions or other methods.

The package is primarily dedicated to near infrared spectral (NIRS) data but can be used for many other types of data.

### Main features

* Data checking and summarizing
* Data pre-processing (detrend, SNV, derivations)
* Multivariate factorial analyses: PCA, PLS, FDA
* Global predictive methods
    - Quantitative regressions: PCR, PLSR
    - Discrimination: PLSDA with various DA methods
* Locally weighted predictive methods
    - Quantitative regressions: kNN-LWPLSR, kNN weighted regressions
    - Discrimination: kNN-LWPLSDA with various DA methods, kNN weighted discrimination
    - The generic function **locw** can implement any locally weighted models
* Cross-validation
* Graphics
* Other functions

Using Rstudio is recommended (https://www.rstudio.com/products/rstudio/download/).


### Installation

1. Install **devtools** package (if not already done)

```{r}
install.packages(devtools)
```

2. Load **devtools** and then install **rnirs** package

```{r}
library(devtools)
install_github("mlesnoff/rnirs", build_vignettes = TRUE, dependencies = TRUE)
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


