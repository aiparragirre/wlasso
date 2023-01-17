# wlasso

The goal of this repository is two-fold: 

- To put publicly available the **R package** `wlasso`. This package allows to fit linear and logistic regression models to complex survey data.
- The R code of the **simulation study** that analyzes the performance of replicate weights' methods to define training and test sets to select optimal LASSO regression models is also available.

Note that all the repository can be **downloaded** from `Code > Download ZIP`.

## R package - wlasso

The R package related to the paper is available in the folder `wlasso`. File `usage-package.R` provides an example of usage of the package functions. Data available in the folder `example-data` can be used to reproduce the examples proposed in the file `usage-package.R`.

Three functions are available in the package:

- `wlasso`: This is the **main function**. This function allows as to fit LASSO prediction (linear or logistic) models to complex survey data, considering sampling weights in the estimation process and selects the lambda that minimizes the error based on different replicate weights methods.
- `plot.wlasso`: plots objects of class `wlasso`, indicating the estimated error of each lambda value and the number covariates of the model that minimizes the error.
- `replicate.weights`: allows to randomly define training and test sets by means of the replicate weights' methods analyzed throughout the paper. The function `wlasso` depends on this function to define training and test sets. In particular, the methods that can be considered by means of this function are: 
  - The ones that depend on the function `as.svrepdesign` from the `survey` package: Jackknife Repeated Replication (`JKn`), Bootstrap (`bootstrap` and `subbootstrap`) and Balanced Repeated Replication (`BRR`).
  - New proposals: Design-based cross-validation (`dCV`), split-sample repeated replication (`split`) and extrapolation (`extrapolation`).

## R code of the simulation study

All the R code needed to reproduce the results obtained in the simulation study of the paper is available in the folder `R code - simulation study`.
