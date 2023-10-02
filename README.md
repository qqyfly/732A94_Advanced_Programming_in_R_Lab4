# 732A94 Advanced Programming in R Lab4
732A94 Advanced Programming in R Lab4

<!-- badges: start -->
  [![R-CMD-check](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab4/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
  # Overview

The `lab04` package provides a lightweight framework for simple linear regression in R. This vignette serves as a guide to understanding and using the main functions provided by the package.

# Installation

To install the `lab04` package, use the following commands:

```r
devtools::install_github("qqyfly/732A94_Advanced_Programming_in_R_Lab4", build_vignettes = TRUE)
```

# Linear Regression with Linreg

The core function in the Linreg package is linreg S3 class, which initializes a linear regression model with all the caculated values stored as a list. 

This example demonstrates how to create a linreg object and how to use all the exported functions of this linreg S3 class.

## Load required libraries

In this step, we need to load the installed library `lab04`.
The code is showed below.

The required packages will be download and installed automatically.

```r
library(lab04)
```

## Create a linreg object
Let's create a simple linear regression model using $iris$ dataset with the formula: $Petal.Length \thicksim Species$.

```r
linreg_mod <- linreg(Petal.Length ~ Species, data = iris)
```

## print a Linreg object
This function use the pre created linreg object as a parameter, and print out some information of the regression model.

```r
print(linreg_mod)
```

## plot a Linreg object
This function plot 2 graphs and show a customized liu logo and liu theme

```r
plot(linreg_mod)
```

## get residuals of regression model
This function use the pre created linreg object as a parameter, and return the residuals values of regression model.

```r
resid(linreg_mod)
```

## get fitted values of regression model
This function use the pre created linreg object as a parameter, and return the fitted values of regression model.

```r
pred(linreg_mod)
```

## get coef values of regression model
This function use the pre created linreg object as a parameter, and return the coef values of regression model.

```r
coef(linreg_mod)
```

## print out the summary of regression model
This function use the pre created linreg object as a parameter, and print out the formula,data name,fitted values,coefficients,Std. Error,p-value,t-value and corresponding visual indicators using p-value.

```r
summary(linreg_mod)
```