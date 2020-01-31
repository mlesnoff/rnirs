## rnirs 1.4-0
## Locally Weighted PLS Regression and Discrimination

R package focusing on locally weighted prediction methods

- Regression for quantitative responses
- Discrimination for qualitative responses 

The prediction models can use dimension reduction methods (e.g. partial least squares) or other methods.

The package can handle many types of data, although it was primarily dedicated to near infrared spectral (NIRS) data.

The changes in the successive versions of the package are reported in the **NEWS** file above.

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

### Main features 

* Data checking and summarizing
* Data pre-processing: Detrend (poly, lowess, als), SNV, Derivation (Savitsky-Golay, Finite difference)
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
    - Block-selection
    - Block-scaling
    - Block-dimension reduction by PLS or PCA
    - Block-dimension reduction by SO-PLS or SO-PCA
* Stacking
    - stackavg
    - stackavg.cla
* Cross-validation
    - Generic function **fitcv**
* Graphics
* Miscellaneous functions

You can write **??rnirs** in the R console for details.

### Installation from Github

1. For the first installation and if not already done

a. For Windows, install Rtools

Download the Rtools.exe available at

https://cran.r-project.org/bin/windows/Rtools/

and run it.

The Rtools site gives indications for other operating systems. 

b. Install Rstudio

https://www.rstudio.com/products/rstudio/download/

c. Install package **devtools**  from the CRAN. Package devtools uses Rtools for facilitating packages installations. Use the Rstudio menu or write in the R console

```{r}
install.packages("devtools")
```
2. Load **devtools** and install **rnirs** package. Write in the R console

```{r}
library(devtools)
install_github("mlesnoff/rnirs", dependencies = TRUE)
```

In case of the following question during installation
```{r}
These packages have more recent versions available.
Which would you like to update?"
```
it is recommended to skip updates.

### Installation from the Tar file rnirs_....tar.gz with Rstudio

1. First, install packages from the CRAN

- **data.table**
- **FNN**
- **ggplot2**
- **gridExtra** 
- **ptw**
- **scales**
- **signal**

2. Then, install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")

### Usage

Load **rnirs**. Write in the R console

```{r}
library(rnirs)
```
To get an overview of the available functions, write in the R console

```{r}
??rnirs
```
### Contact

Matthieu Lesnoff
matthieu.lesnoff@cirad.fr

