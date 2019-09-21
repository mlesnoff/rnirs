# R package rnirs 1.0-7  
#### Locally Weighted PLS Regression and Discrimination

Package **rnirs** is a statistical tool box focusing on locally weighted prediction methods: regression for quantitative responses and discrimination for qualitative responses. The predictive models can use partial least squares (PLS) dimension reductions or other methods.

The package is primarily dedicated to near infrared spectral (NIRS) data but can be used for many other types of data.

#### Main features

* Data checking and summarizing
* Data pre-processing
* Multivariate factorial analyses: PCA, PLS, FDA
* Global predictive methods
    - Quantitative regressions: PCR, PLSR
    - Discrimination: PLSDA with various DA methods
* Locally weighted predictive methods
    - Quantitative regressions: kNN-LWPLSR, kNN weighted regressions
    - Discrimination: kNN-LWPLSDA with various DA methods, kNN weighted discrimination
    - Function locw can implement any locally weighted models
* Cross-validation
* Graphics
* Other functions

Under Windows, the computations made in the package are faster with the R version "Microsoft R Open" (https://mran.microsoft.com/open). Installing Rstudio (https://www.rstudio.com/products/rstudio/download/) is also recommended.


#### Installation (GithHub package version)

Install devtools package if not already done

```{r}

install.packages(devtools)

```
Load devtools then install rnirs package

```{r}

library(devtools)
install_github("mlesnoff/rnirs", build_vignettes = TRUE)

```
Then load rnirs

```{r}

library(rnirs)

```

To get an overview of the available functions

```{r}

??rnirs

```






