## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 04
##
## Date Created: 2023-09-25
##
## Copyright (c) MIT
## ---------------------------

#' A S3 class provide linear regression  that is similar as lm().
#'
#' @name linreg
#' @param formula Formula
#' @param data Dataframe
#' @return linreg object(list) contain all the calculated values regarding the linear regression
#' @importFrom stats model.matrix pt
#' @export 
linreg <- function(formula, data) {
  
  # object to save all the meta data and calculated values
  obj <- list(formula = formula,
              data = data,
              data_name = substitute(data),
              coefficients = NULL,
              residuals = NULL,
              fitted_values = NULL,
              residual_var = NULL,
              regressions_var_diag = NULL,
              regressions_var = NULL,
              t_values = NULL,
              p_values = NULL,
              x_name = NULL,
              x_data = NULL,
              df = NULL)
              
  # set class name
  class(obj) <- "linreg"
  
  #----------------START OF INIT CODE----------------
  
  # get all the variables
  vars <- all.vars(formula)
  
  # get dependent variable name and data
  y_name <- vars[1]
  y_data <- as.matrix(data[y_name])
  
  # create the design matrix X
  x_data <- model.matrix(formula, data)
  obj$x_name <- vars
  obj$x_data <- x_data
  
  # transpose of X_data and other calculation for later usage
  x_data_t <- t(x_data)
  xtx_inv <- solve(x_data_t %*% x_data)
  
  # calculate regressions coefficients
  coefficients <- (xtx_inv %*% x_data_t) %*% y_data
  obj$coefficients <- coefficients
  
  # calculate fitted values 
  fitted_values <- x_data %*% coefficients
  obj$fitted_values <- fitted_values 
  
  # calculate residuals
  residuals <- y_data - fitted_values
  obj$residuals <- residuals
  
  # calculate degrees of freedom
  df <- length(x_data[,1]) - length(x_data[1,])
  obj$df <- df
  
  # calculate residual variance
  residual_var <- (t(residuals) %*% residuals) / df
  obj$residual_var <- residual_var
  
  # calculate variance of the regression coefficients:
  regressions_var <- as.numeric(residual_var) * xtx_inv
  obj$regressions_var <-regressions_var
    
  # calculate t-value
  
  # get the diagonal of regressions_var matrix
  regressions_var_diag <- diag(regressions_var)
  obj$regressions_var_diag <- regressions_var_diag
  t_values <- coefficients / sqrt(regressions_var_diag)
  obj$t_values <- t_values
  
  # calculate p-value
  p_values <- 2 * pt(abs(t_values), df, lower.tail = FALSE)
  obj$p_values <- p_values
  
  #----------------END OF INIT CODE----------------
  obj
}

#' print out formula, data and Coefficients of current linear regression.
#'
#' @param x linreg object
#' @param ... other objects
#' @export print.linreg
#' @export 

print.linreg <- function(x,...) {
  obj <- x
  cat("Call:\n")
  data_str <- paste("data = ",obj$data_name, ")", sep="")
  formula_str <- paste("linreg(formula =",deparse(obj$formula))
  
  firstPart <- paste(formula_str,data_str, sep=", ")
  cat(firstPart)
  
  cat("\n\nCoefficients:\n")
  
  cat(colnames(obj$x_data))
  cat("\n")
  cat(obj$coefficients)
}

#' get PNG file(used in plot())
#' private function
#' @param filename png file name used in plot
get_png <- function(filename) {
  grid::rasterGrob(readPNG(filename), interpolate = TRUE)
}

#' customize Liu theme (used in plot())
#' private theme
#'
linkoping_theme <- theme(
    panel.background = element_rect(fill = "white",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(color="blue2", face="bold", size="14",hjust = 0.5)
)

#' plot 2 graphs with Liu Logo and customized liu theme.
#'
#' @param x linreg objec
#' @param ... graphical parameters to plot
#' 
#' @importFrom ggplot2 ggplot geom_path geom_point aes_string aes theme element_text element_rect element_line stat_summary labs theme_void annotation_custom
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @importFrom gridExtra  grid.arrange
#' 
#' @export plot.linreg
#' @export 
#' 
plot.linreg <- function(x, ...) {
  obj <- x

  l <- get_png("LiU.png")
 
  # setup data frame for 1st graph
  fitted_values <- as.data.frame(obj$fitted_values)
  residuals <- as.data.frame(obj$residuals)
  df_g1 <- data.frame(fitted_values,residuals)
 
  # draw 1st graph
  p1 <- ggplot2::ggplot(data = df_g1,mapping = aes_string(x = names(df_g1)[1], y = names(df_g1)[2])) +
    geom_point(shape = 21) +
    stat_summary(fun="median",geom="line",linetype = "dashed",colour="red") +
    linkoping_theme +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values \n lm(Petal.Length ~ Species)",
      y = "Residuals"
    )

  # calc the mean of residuals-> miu and set up data frame for 2nd graph
  miu <- mean(residuals[[names(residuals)[1]]])
  sigma <- sqrt(sum((residuals - miu) ^ 2) / nrow(residuals))

  residuals_new <- abs((residuals - miu) / sigma)
  df_g2 <- data.frame(fitted_values,residuals_new)
  
  # draw 2nd graph
  p2 <- ggplot2::ggplot(data = df_g2,mapping = aes_string(x = names(df_g2)[1], y = names(df_g2)[2])) +
    geom_point(shape = 21) +
    stat_summary(fun="median",geom="line",linetype = "dashed",colour="red") +
    linkoping_theme + 
    labs(
      title = "Scale Location",
      x = "Fitted values \n lm(Petal.Length ~ Species)",
      y = expression(paste(sqrt(abs("Standard Residuals"))))
    )

  # draw 3rd graph (LOGO)
  p3 <- ggplot(mapping = aes(x = 0:1, y = 0.1)) +
      theme_void() +
      annotation_custom(l, xmin = .8, xmax = 1)
  grid.arrange(p1, p2, p3,heights = c(.46,.46,.08))
}

#' get fitted value
#'
#' @param obj linreg object
#' @export pred
#' @export
pred <- function(obj){
  return(obj$fitted_values)
}

#' get residuals
#'
#' @param obj linreg object
#' @export resid
#' @export
 
resid <- function(obj){
  obj$residuals
}

#' get coefficients values
#'
#' @param obj linreg object
#' @export coef
#' @export

coef <- function(obj){
  obj$coefficients
}


#' print out summary which is simiar as lm()
#'
#' @param object linreg object
#' @param ... other objects
#' @importFrom methods show
#' @export summary.linreg
#' @export 
summary.linreg <- function(object, ...){
  obj <- object
  cat("Call:\n")
  data_str <- paste("data = ",obj$data_name, ")", sep="")
  formula_str <- paste("linreg(formula =",deparse(obj$formula))
  firstPart <- paste(formula_str,data_str, sep=", ")
  cat(firstPart)
  cat("\n\n")
  
  cat("Coefficients:\n")
  star <- lapply(obj$p_values[1],check_p_value)
  secondPart <- data.frame(obj$coefficients,
                           sqrt(obj$regressions_var_diag),
                           obj$t_values,
                           formatC(obj$p_values, format = "e", digits = 2),
                           star
                           )
  
  names(secondPart) <- c("Estimate","Std. Error","t value","Pr(>|t|)"," ")
  show(secondPart)
  
  cat("\n\n")
  # Residual standard error
  residual_standard_error_str <- paste("Residual standard error: ",sqrt(obj$residual_var)," on ",obj$df , " degrees of freedom",sep="")
  cat(residual_standard_error_str)
}

#' return confidence indicator[. * ** ***] according to the value X
#'
#' @param x numeric
#' 
check_p_value <- function(x){
   if (x > 0.1){
     return("")
   }else if (x > 0.05){
     return(".")
   }else if (x > 0.01){
     return("*")
   }else if (x > 0.001){
     return("**")
   }else{
     return("***")
   }
}
