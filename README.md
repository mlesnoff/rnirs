## rnirs 1.6-5
## Chemometrics Methods and Locally Weighted Regressions 

R package focusing on chemometrics methods, including locally weighted PLS regression and discrimination.

The package is generic for any type of data, although it was initially implemented for near infrared spectral data (NIRS).

The changes in the package versions are reported in the **NEWS** file above.

Using Rstudio is recommended for installation and usage (https://www.rstudio.com/products/rstudio/download/).

## Main features 

* Data checking and summarizing
* Data pre-processing: Detrend (poly, lowess, als), SNV, Derivation (Savitsky-Golay, Finite difference)
* Multivariate factorial analyses 
    - PCA: Usual, Robust
    - PLS
    - FDA
* Variable selection: CovSel
* Regression
    - LMR
    - PLSR
    - KNNWR, LWPLSR, KNN-LWPLSR (wrappers using function locw)
* Discrimination (with various DA methods)
    - PLSDA
    - KNNWDA, LWPLSDA, KNN-LWPLSDA (wrappers using function locw)
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

## Installation from Github

**Warning:** The new R base version 4.0.0 requires re-installing all the packages. The easiest way is to remove all the folders where have been installed the packages by the previous version, and then open Rstudio, going in the Package menu and re-installing them automatically with dependencies.

**A. Only for the first installation and if not already done** 

**1. Install Rstudio**

https://rstudio.com/products/rstudio/download/ 

**2. Install the R package devtools from CRAN.** 

Package devtools is used for installing other R packages (such as rnirs) from Github.  

Use the Rstudio menu or write in the R console
```{r}
install.packages("devtools")
```

**3. Install Rtools**

The Rtools program is required for running package devtools.

Two situations:

a. For R version >= 4.0.0

- Download Rtools40.exe at https://cran.r-project.org/bin/windows/Rtools/ and run it.

- Create a text file ".Renviron" (if not already existing) in your "Documents" folder and copy it the following line
```{r}
PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
```

b. For R versions <= 3.4.6

Download the Rtools.exe available at https://cran.r-project.org/bin/windows/Rtools/history.html and run it.

**B. After this first installation** 

**Load devtools and install rnirs package** 

Write in the R console
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

## Installation from the archive file rnirs_....tar.gz with Rstudio

A. First, install the following packages from the CRAN

- **data.table**
- **FNN**
- **ptw**
- **signal**

B. Then, install the file **rnirs_....tar.gz** (Menu "Packages", "Install", and then "Install From Package Archive Files")

## Usage

Load **rnirs**. Write in the R console

```{r}
library(rnirs)
```
**To get an overview of the available functions**

Write in the R console

```{r}
??rnirs
```
### Contact

Matthieu Lesnoff
matthieu.lesnoff@cirad.fr

