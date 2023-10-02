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
#' Author: Qinyuqn Qi
#' @param formula Formula
#' @param data Dataframe
#' @param qr Boolean
#' @return linreg object(list) contain all the calculated values regarding the linear regression
#' @export linreg
#' @export 
#' 
#' 
# Document the myclass class
#' @export
#' @name myclass
#' @title My S3 Class
#' @description This is a simple S3 class for demonstration purposes.
#' @field data A numeric vector.

linreg <- function(formula, data, qr = FALSE) {
  
  # object to save all the meta data and calculated values
  obj <- list(formula = formula,
              data = data,
              qr = qr,
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
  return(obj)
}

#' print out formula, data and Coefficients of current linear regression.
#'
#' Author: 
#' @param obj linreg object
#' @export print.linreg 
#' @export 
print.linreg <- function(obj) {
  
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
#' Author: 
#' @param filename png file name used in plot
get_png <- function(filename) {
  grid::rasterGrob(readPNG(filename), interpolate = TRUE)
}

#' plot 2 graphs with Liu Logo and customized liu theme.
#'
#' Author: 
#' @param obj linreg object
#' @export plot.linreg
#' @export
plot.linreg <- function(obj) {
  
  linkoping_theme <-
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"),
      plot.title = element_text(color="blue2", face="bold", size="14"),
    )
  
  l <- get_png("LiU.png")
  
  # Create moving median here, and window size set to 5
  window_size <- 5

  # setup data frame for 1st graph
  fitted_values <- as.data.frame(obj$fitted_values)
  residuals <- as.data.frame(obj$residuals)
  df_g1 <- data.frame(fitted_values,residuals)
  length_moving_median <- length(residuals[,1]) - window_size + 1
  
  # Calculate the moving median values
  residuals_moving_median <- sapply(1:length_moving_median, function(i) {
    median(residuals[i:(i + window_size - 1),1])
  })
  
  # draw 1st graph
  p1 <- ggplot(data = df_g1,mapping = aes_string(x = names(df_g1)[1], y = names(df_g1)[2])) +
    geom_point(shape = 21) +
    geom_line(data = data.frame(x = fitted_values[1:length_moving_median,1], y = residuals_moving_median), aes(x, y), color = "red", linetype = "dashed") +  # Add a red dashed moving median line
    stat_smooth(method='lm', colour="blue", se=FALSE, span = 1,linetype = "dashed") +
    linkoping_theme +
    
    theme(plot.title = element_text(hjust = 0.5)) +
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
  
  # calculate  moving median
  length_new_median_value <- length(residuals_new[,1]) - window_size + 1
  #Calculate the moving median values
  residuals_new_moving_median <- sapply(1:length_new_median_value, function(i) {
    median(residuals_new[i:(i + window_size - 1),1])
  })
  
  # draw 2nd graph
  p2 <- ggplot(data = df_g2,mapping = aes_string(x = names(df_g2)[1], y = names(df_g2)[2])) +
    geom_point(shape = 21) +
    geom_line(data = data.frame(x = fitted_values[1:length_new_median_value,1], y = residuals_new_moving_median), aes(x, y), color = "red", linetype = "dashed") + 
    stat_smooth(method='lm', colour="blue", se=FALSE, span = 1,linetype = "dashed") +
    theme(plot.title = element_text(hjust = 0.5)) +
    linkoping_theme + 
    labs(
      title = "Scale Location",
      x = "Fitted values \n lm(Petal.Length ~ Species)",
      y = expression(paste(sigma,sqrt(abs("Standard Residuals"))))
    )

  # draw 3rd graph (LOGO)
  p3 <- ggplot(mapping = aes(x = 0:1, y = 0.1)) +
      theme_void() +
      annotation_custom(l, xmin = .8, xmax = 1)
  
  # vertical layout (1 X 3) with customized height in percent
  grid.arrange(p1, p2, p3,heights = c(.46,.46,.08))
}

#' get residuals
#'
#' Author: 
#' @param obj linreg object
#' @export resid.linreg 
#' @export
resid.linreg <- function(obj){
  return(obj$residuals)
}

#' get fitted value
#'
#' Author: 
#' @param obj linreg object
#' @export pred.linreg 
#' @export
pred.linreg <- function(obj){
  return(obj$fitted_values)
}

#' get coefficients values
#'
#' Author: 
#' @param obj linreg object
#' @export coef.linreg
#' @export
coef.linreg <- function(obj){
  return(obj$coefficients)
}

#' print out summary which is simiar as lm()
#'
#' Author: 
#' @param obj linreg object
#' @export summary.linreg
#' @export
summary.linreg <- function(obj){
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
#' Author: 
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
