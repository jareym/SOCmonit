#' Plot PLS Regression Results
#'
#' Function to visualize the results of PLS regression.
#'
#' @param model.pls The PLS regression model object.
#' @return None
#' @export
#'
#' @examples
#' # Assuming model_pls is a PLS regression model object
#' plot.pls.results(model_pls)
#'
pls.results.plot <- function(model.pls) {
  num_reps <- ncol(model_pls$Predictions) - 2  # number of repetitions

  min_val <- min(model_pls$Predictions$soc.value)
  max_val <- max(model_pls$Predictions$soc.value)

  # 1. Scatter plot of actual vs. predicted values
  par(mfrow = c(2, 3))
  for (i in 1:num_reps) {
    plot(model_pls$Predictions$soc.value, model_pls$Predictions[[i+2]],
         xlim = c(min_val, max_val), ylim = c(min_val, max_val),
         xlab = "Actual values", ylab = "Predicted values",
         main = paste("Rep.", i),
         asp = 1)  # Keep the aspect ratio 1:1
    abline(a = 0, b = 1, col = "red")  # Add a y=x line for reference
  }

  # 2. Residual plots
  par(mfrow = c(2, 3))
  for (i in 1:num_reps) {
    residuals <- model_pls$Predictions$soc.value - model_pls$Predictions[[i+2]]
    plot(model_pls$Predictions[[i+2]], residuals,
         xlab = "Predicted values", ylab = "Residuals",
         main = paste("Rep.", i))
    abline(h = 0, col = "red")  # Add a line at zero for reference
  }

  # Reset graphical parameters for the bar plot
  par(mfrow = c(1, 1))

  # 3. Bar plot for optimal number of components
  opt_components <- unlist(model_pls$`Opt. Comp.`)  # Unlist the components
  barplot(opt_components,
          main = "Optimal number of components",
          xlab = "Model repetition", ylab = "Number of components",
          names.arg = names(opt_components)) # Use the names from opt_components
}
