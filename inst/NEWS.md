## NEWS for package **rnirs**

## **Version 1.9-4**

#### **New functions**

- Degrees of freedom for PLSR1 models

  - **plsrdf.cov**
  - **plsrdf.div**

- Mallows Cp for PLSR1 models

  - **plsrdf.cov**

#### **Modified functions**

- **plotxna**: New argument "grid"

- **pls**, **plsr**, **plsdalm**, **plsda**: argument "algo" set to NULL (this has no implication)





  



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








