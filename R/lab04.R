library(ggplot2)
library(MASS)

#' calc lm, and return all the variable related to it.
#'
#' @param formula A formula
#' @param data A data frame

#' @examples
#' # example code
#' data(iris)
#' mod_object <- linreg(Petal.Length~Species, data = iris)
#' or even this
#' mod_object <- linreg(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' 
#' @return The linreg S3 class
linreg <- function(formula, data, qr = FALSE) {
  # assign the class name
  class(linreg_object) <- "linreg"
  
  # Create the design matrix X
  m <- model.matrix(formula - 1, data)
  
  # get all the variable of the formula
  # dependent variables number will always be 1
  # independent variables number may >= 1
  vars <- all.vars(formula)
  
  # get the number of variables
  n <- length(all.vars(formula))
  
  # get the dependent variables name and data
  y_name <- vars[1]
  y_data <- data[vars[1]]
  
  # get the independent variables name and data
  X_name <- vars[2:n]
  X_data <- data[vars[2:n]]
  
  # transpose of X_data
  X_data_t = t(as.matrix(X_data))
  
  # calc coef here
  
  # use ginv to calc Generalized Inverse of a Matrix instead of solve
  # because solve will generate singular problem
  
  beta <- tcrossprod(ginv(crossprod(x = as.matrix(X_data_t))),X_data_t)
  
  # calc residuals here
  
  
  

  # Create a linreg object, and return
  linreg_object <- list(
    coefficients = coef(fit),
    residuals = residuals(fit),
    fitted.values = fitted(fit),
    call = match.call(),
    formula = formula,
    data = data,
    terms = terms(fit)
  )
  
  return(linreg_object)
}

#' return resid that restored in linreg object
#'
#' @param x linreg class object
#'
#' @return resid
resid <- function(x){
  if (!inherits(x, "linreg")) {
    stop("Input object is not of class 'linreg'")
  }
  
  return(x$linreg_object.residuals)
}

#' return pred that restored in linreg object
#'
#' @param x linreg class object
#'
#' @return pred
pred <- function(x){
  if (!inherits(x, "linreg")) {
    stop("Input object is not of class 'linreg'")
  }
}

#' return coef that restored in linreg object
#'
#' @param x linreg class object
#'
#' @return coef
coef <- function(x){
  if (!inherits(x, "linreg")) {
    stop("Input object is not of class 'linreg'")
  }
}

#' print summary informationof linreg object
#'
#' @param x linreg class object
summary <- function(x){
  if (!inherits(x, "linreg")) {
    stop("Input object is not of class 'linreg'")
  }
}

# Summary function for linreg class
summary.linreg <- function(object, ...) {
  cat("Custom Linear Regression Model\n")
  cat("\nCall:\n")
  print(object$call)
  
  cat("\nCoefficients:\n")
  print(object$coefficients)
  
  cat("\nResiduals:\n")
  print(object$residuals)
  
  cat("\nFitted Values:\n")
  print(object$fitted.values)
  
  cat("\nTerms:\n")
  print(object$terms)
  
  return(invisible(NULL))
}



# Define a method to print information
print.linreg <- function(obj) {
  #cat("Call:\n","m(formula = ",Reduce(paste, deparse(obj$formula)),"data = ",deparse(substitute(obj$data)))
  k<- deparse(substitute(obj$data))
  cat(k)
}
