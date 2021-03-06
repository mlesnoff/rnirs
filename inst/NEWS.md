## NEWS for package **rnirs**

## **Version 1.9-18**

- **orthog**: Internal modification  





## **Version 1.9-16**


- New function: **xinterp** (Resampling of spectra by nterpolation methods)

- Modification of functions 

  - **scordis** and **odis**: cutoff value calculation has changed
  - Arguments of **outsdod**, **pca_rob** and **pls_rob** have changed





## **Version 1.9-15**


- Function **cpplsr** has been removed and replaced by **aicplsr**

- Internal modifications in functions **dfplsr_cov**, **dfplsr_div**





## **Version 1.9-14**


- Modification of function **cpplsr**. See help page.

- Internal modifications in functions **dfplsr_cov**, **dfplsr_div** and **seldesign**





## **Version 1.9-13**

- Correction of small typo bugs in function **cppca**

- Improvements and modifications **selangle** and **selcoll**; See the corresponding help pages

- Code cleaning





## **Version 1.9-12**

- Function **selcoef** was removed (possibly temporrary)




## **Version 1.9-11**

- Modification of **selangle**




## **Version 1.9-10**

#### **New functions**

- Graphic methods for selecting PCA or PLS model dimension

  - **selbroken**
  - **selhorn**
  - **selkaiser**
  - **selkarlis**
  - **selscree**
  
  - **selangle**
  - **selcoef**
  - **selcoll**
  
- Graphic methods for selecting regression models 
  
  - **selsign**
  - **selwik**

- Auxiliaray function

  - **plotsl**

#### **Renamed functions**

- **cvpcackf** to  **cvpca_ckf**
- **cvpcaia** to  **cvpca_ia**
- **cvpcatri** to  **cvpca_tri**
- **cvpcarw** to  **cvpca_rw**

- **dfpcadiv** to **dfpca_div**
- **dfplscov** to **dfpls_cov**
- **dfplsdiv** to **dfpls_div**

- **pcasph** to  **pca_sph**
- **pcacr** to **pca_cr** 
- **pcarob** to **pca_rob**

#### **Modified functions**

- **pca**: Row "ncomp=0" removed in output "explvar"

- Code cleaning






## **Version 1.9-9**

- Code cleaning






## **Version 1.9-8**

- Gram-Schmidt orthogonalization has been added to function **pca_nipals**, 
and modified in **pca_nipalsna**

- Code cleaning








## **Version 1.9-7**

- Major changes of function names: For consistency with S3 rules, all "." in the function names have been replaced by "" or "_"

- Code cleaning








## **Version 1.9-6**

#### **New functions**

  - **cvpca.rw**: Row wise PCA Cross-validation
  - **dfpca.div**: Degrees of freedom of PCA models
  - **cppca**: Mallows Cp for PCA models
  
#### **Modified functions**

  - **summ**: outputs changed





## **Version 1.9-5**

#### **Renamed functions**

  - **dfplsr.cov** to **dfplsr.cov**
  - **dfplsr.div** to **dfplsr.div**
  - **plsrcp** to **cpplsr**





## **Version 1.9-4**

#### **New functions**

- Degrees of freedom for PLSR1 models

  - **plsrdf.cov**
  - **plsrdf.div**

- Mallows Cp for PLSR1 models

  - **plsrcp**

#### **Modified functions**

- **plotxna**: New argument "grid"

- **pls**, **plsr**, **plsdalm**, **plsda**: argument "algo" set to NULL (this has no implication)

#### **New data set**

- **datozone**






## **Version 1.9-3**

#### **Renamed functions**

- **fitcv** to **cvfit** 

#### **New functions**

- Missing data imputation (MDI) in a matrix

  - **ximput.ia**: IA algorithm
  
  - **plotxna**: Plotting missing data in a matrix
  
- CV for PCA models

  - **cvpca.ia**: PCA CV with IA algorithm

  - **cvpca.tri**: PCA CV with ekf-TRI algorithm
  
  - **cvpca.trickf**: PCA CV with ckf-TRI algorithm
  
#### **Modified functions**

- **pca.nipalsna**: Argument "gramschmidt" renamed to "gs", argument "maxit"  
increased to 200, plus internal changes 

- **selwold**: Argument "typ" renamed to "type"

#### **New data set**

- **datoctane**




  



## **Version 1.9-2**

#### **New functions**

- **xssr**: Sum of squared X-residuals from scores and loadings

#### **Modified functions**

- **blocksopls**, **blocksopca**: Input "ncomp" can now have components = 0. 
Thanks to Evelyne Vigneau (Oniris France)

- **dasdod**: Meaning of parameter **theta** has changed (before was **1 - theta**)

- **xfit**: Internal changes

- **mse**: Internal changes

- **err**: Internal changes








## **Version 1.9-1**

#### **Modified functions**

- **selwold**: The function has changed. Only vectors are allowed as input. More generic. New plots.








## **Version 1.9-0**

#### **Renamed functions**

- **kentson** to **sampks**
  
- **selncomp.wold** to  **selwold**  (with slight modification)
  
- **segmcvkfold** to  **segmkf**                   
- **segmcvmc** to  **segmts**                   

#### **New functions**

- **plotjit**: Jittered plot
  
- **sampdp**: Duplex sampling algorithm 
  
#### **New arguments**

- Argument **ncomp** added to functions **scordis** and **odis**
  
- Argument **alpha.f** (color opacity for points) added function **plotxy**
  
- Argument **weights** added to function **daglm**
  
- Argument **alpha** in functions **pca.rob** and **pls.rob** has changed of definition (was **1 - alpha** before)

#### **New outputs**

- Output **eig** in functions **pca.eigen**, etc. now contains the full set of eigenvalues (not only the first **ncomp** values)
  
- Output **explvarx** in functions **pca** and **kpca** has been renamed to **explvar**
    
#### **Other**

- Function **sampclas** has changed

- Re-organisation of some outputs in discrimination functions








## **Version 1.8-7**

#### **New functions**

- **svmr**: SVM regression
  
- **svmc**: SVM discrimination (classification)
  
- **splitpar**: Auxiliary function for tuning models








