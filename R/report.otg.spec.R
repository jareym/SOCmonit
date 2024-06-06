#' Generate a report summarizing the results of preprocessing, modeling and spatial interpolation of on-the-go spectral data for SOC prediction.
#'
#' This function generates a comprehensive report that presents the results obtained from applying the SOCmonit package on on-the-go spectral data for predicting Soil Organic Carbon (SOC). The report covers various aspects, such as reading and testing the spectral data, preprocessing the spectral data, reading the soil data, cross-validation strategy with k-fold partitioning, Partial Least Squares (PLS) modeling, variogram modeling and kriging spatial interpolation, and visualization of the results.
#'
#' If TinyTeX is not installed on the system, this function will automatically install it to ensure proper LaTeX compilation for generating the report.
#'
#' @param output_dir The directory where the report will be saved.
#' @param output_file The name of the output file without the file extension.
#' @param spectra_data The spectral data with corrected values.
#' @param test_results Results of testing the spectral data.
#' @param soil_data The soil data used for SOC prediction.
#' @param sampling_points A scatter plot representing the sampling points.
#' @param pp_data The preprocessed spectral data.
#' @param fold_list The k-fold partitions for cross-validation.
#' @param model_pls The results of the PLS modeling.
#' @param model_vg The variogram model used for spatial interpolation.
#' @param model_ok The results of the ordinary kriging interpolation.
#'
#' @return The path to the generated PDF report.
#'
#' @examples
#' # Generate a report with all sections
#' report.otg.spec(output_dir = "report", output_file = "summary",
#'                   spectra_data = spectra_data, test_results = test_results,
#'                   soil_data = soil_data, sampling_points = sampling_points,
#'                   pp_data = pp_data, fold_list = fold_list, model_pls = model_pls,
#'                   model_vg = model_vg, model_ok = model_ok)
#'
#' # Generate a report with specific sections
#' report.otg.spec(output_dir = "report", output_file = "summary",
#'                   spectra_data = spectra_data, soil_data = soil_data, model_vg = model_vg)
#'
#' @importFrom rmarkdown render
#' @import sp
#' @export


report.otg.spec <- function(output_dir, output_file,
                            spectra_data = NULL, test_results = NULL,
                            soil_data = NULL, sampling_points = NULL,
                            pp_data = NULL, fold_list = NULL,
                            model_pls = NULL, model_vg=NULL, model_ok=NULL) {

  if (!tinytex::is_tinytex()) {
    message("Installing TinyTeX...")
    tinytex::install_tinytex()
  }
  
# Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Set the Rmd file path
  rmd_path <- file.path(output_dir, "report.Rmd")
  # Set the PDF file path
  pdf_path <- file.path(output_dir, paste0(output_file, ".pdf"))

  # Create a new Rmd file
  rmd_content <- '
---
title: "Preprocessing and Modeling on-the-go spectral Data  for SOC Prediction"
output: pdf_document
---

# Introduction

This report provides a summary of the results obtained using the SOCmonit package for preprocessing and modeling on-the-go spectral data for SOC prediction. It covers various steps, including reading and testing point spectral data, reading soil data, preprocessing ASD data, creating a k-fold list for cross-validation, applying PLS modeling, applying kriging spatial interpolation and visualizing the results.
'

if (!is.null(spectra_data)) {
    rmd_content <- paste0(rmd_content, '

# Reading and Testing Point Spectral Data

Description of the spectral data:

The `spectra_data` object contains the spectral data with corrected values. It is a data frame with the following columns:

- `ID.point`: The identifier for each point.
- `ID.sample`: The identifier for each sample.
- `spec`: The spectral data columns.

The analysis of the point spectral data reveals the following insights:

```{r}
spectra_data[1:10,1:10]

```
')
}
  if (!is.null(soil_data)) {
    rmd_content <- paste0(rmd_content, '
# Reading Soil Data


Description of the soil data:

The `soil_data` object contains the soil data for SOC prediction. It is a data frame with the following columns:

    ID.point: The identifier for each point.
    ID.sample: The identifier for each sample.
    soc: The SOC values.

The analysis of soil data provides the following observations:
```{r}
soil_data
```
')
  }

  if (!is.null(sampling_points)) {
    rmd_content <- paste0(rmd_content, '

  # Reading and Visualizing Sampling Points


Visualization of the sampling points.

The sampling_points object represents the sampling points as a scatter plot of x and y coordinates.

The analysis of sampling points yields the following results:

```{r}
sampling_points
```
')
  }

  if (!is.null(pp_data)) {
    rmd_content <- paste0(rmd_content, '
# Preprocessing on-the-go Data


The pp_data object contains the preprocessed ASD data. It includes the following information:

    ID.point: The identifier for each point.
    ID.sample: The identifier for each sample.
    preprocessed_spec: The preprocessed spectral data columns.

The preprocessing of on the go data leads to the following outcomes:

```{r}
pp_data[1:10,1:10]
```
')
  }

  if (!is.null(fold_list)) {
    rmd_content <- paste0(rmd_content, '

# Creating a K-Fold List

## Expected Output
The `fold_list` object is a list of k-fold partitions for cross-validation. Each partition represents a combination of training and validation sets for model evaluation. The k-fold list provides a robust approach to assess the performance of the SOC prediction models.

The creation of a k-fold list for cross-validation produces the following outcome:
```{r}
head(fold_list[[1]])

```
')
  }

  if (!is.null(model_pls)) {
    rmd_content <- paste0(rmd_content, '

# Applying PLS Modeling

The `model_pls` object represents the results of the PLS modeling.

### Predictions
- A data frame containing the sample ID and the predicted SOC values for each external repetition.

### Models
- A list containing the PLS models for each fold in each external repetition.

### Optimal Number of Components
- A list containing the optimal number of components for each model in each fold.

### Error Metrics
- A data frame containing the RMSE, R2, and RPIQ values for each external repetition.

The application of PLS modeling on the combined spectral and soil data generates the following findings:

```{r}
head(model_pls$Predictions)

model_pls$`Error metrics`
SOCmonit::pls.results.plot(model_pls)

```
')
  }

  if (!is.null(model_vg)) {
    rmd_content <- paste0(rmd_content, '

# Variogram model

Below is the fitted variogram model for our data. The plot presents the experimental variogram points and the fitted theoretical variogram curve. The distance along the x-axis represents the spatial lag between observation pairs, while the semi-variance on the y-axis measures the dissimilarity between those observation pairs.

```{r}
model_vg
plot(model_vg$variogram,  model=model_vg$model,   col="red" )

```
')
  }

  if (!is.null(model_ok)) {
    rmd_content <- paste0(rmd_content, '

# Spatial interpolation

Here we have employed Spatial interpolation using kriging. The figures below display the predicted SOC values (left) and the associated prediction variances (right) in our study area. High prediction variances indicate areas of lower confidence in the predicted values.

```{r}
model_ok
map <- sp::SpatialPixelsDataFrame(model_ok@coords, model_ok@data)

p1 <- sp::spplot(map, "var1.pred", col.regions = terrain.colors(16),
        scales = list(draw = TRUE), main = "Prediction")
p2 <- sp::spplot(map, "var1.var", col.regions = cm.colors(16),
        scales = list(draw = TRUE), main = "Variance")
    gridExtra::grid.arrange(nrow = 1, p1, p2)

```
')
  }
rmd_content <- paste0(rmd_content, '
# Conclusion

This report provides an overview of how we used the SOCmonit package to process and analyze soil and spectral data for better soil organic carbon (SOC) prediction.

The steps we followed included analyzing spectral and soil data, preprocessing the data, creating models for predictions, and using spatial techniques for further insights. The approach proved useful for making reliable predictions about SOC.

The report includes several visual aids to help better understand the findings.
')

  # Write the content to the Rmd file
  cat(rmd_content, file = rmd_path)

  # Convert the Rmd file to a PDF using rmarkdown
  rmarkdown::render(rmd_path, output_file = pdf_path)

  # Remove the Rmd file
  file.remove(rmd_path)

  return(pdf_path)
}
