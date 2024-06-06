#' Generate a report summarizing the results of UAV spectral data analysis.
#'
#' This function generates a report summarizing the results obtained using the SOCmonit package for application of key functions for pre-processing and comparing field spectrometer data with drone data.
#'
#' If TinyTeX is not installed on the system, this function will automatically install it to ensure proper LaTeX compilation for generating the report.
#'
#' @param output_dir The directory where the report will be saved.
#' @param output_file The name of the output file without the file extension.
#' @param UAV_data The UAV image data.
#' @param asd_data The ASD data.
#' @param srf_data The SRF data.
#' @param processing_output The output from the image processing step.
#' @param predrespDS The predictor response dataset.
#' @param Cam_type The type of camera used (Mica or Tetra).
#'
#' @return The path to the generated PDF report.
#'
#' @examples
#' # Generate a report with all sections
#' report.uav(output_dir = "report", output_file = "summary",
#'            UAV_data = UAV_data, asd_data = asd_data, srf_data = srf_data,
#'            processing_output = processing_output, predrespDS = predrespDS, Cam_type = "Mica")
#'
#' # Generate a report with only specific sections
#' report.uav(output_dir = "report", output_file = "summary",
#'            UAV_data = UAV_data, asd_data = asd_data)
#'
#' @importFrom rmarkdown render
#' @export


report.uav <- function(output_dir, output_file,
                            UAV_data = NULL, asd_data = NULL, srf_data = NULL,
                            processing_output = NULL, predrespDS  = NULL, Cam_type=c("Mica", "Tetra")) {

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
title: "Comparison of in situ spectrometer and UAV data"
output: pdf_document
---

# Introduction

This report provides a summary of the results obtained using the SOCmonit package for application of key functions for pre-processing and comparing field spectrometer data with drone data.
'

if (!is.null(UAV_data)) {
    rmd_content <- paste0(rmd_content, '

## Read UAV image

The  function `rd.UAV` can be populated by the user with input paths to the drone image to be read as well as an optional shapefile that is used to clip the image to the according.

```{r}
UAV_data
raster::plot(UAV_data, main="Original Image - Plot bandwise")

```
')
}
  if (!is.null(asd_data)) {
    rmd_content <- paste0(rmd_content, '

## Read ASD data

The `rd.ASD` function offers the user to either just load the data recorded with the analytical spectral device (ASD) spectroradiometer or to additionally conduct a sensor jump correction.
The effect of the ASD sensor jump correction can be visualized using the `ASDjump.plot` function.
The data provides the following observations:

```{r}
#asd data without correction
 asd_data[[1]][1:10,1:10]

#asd data with correction
asd_data[[2]][1:10,1:10]

ASDjump.plot(asd_data[[1]], asd_data[[2]], "001A")

```
')
  }

  if (!is.null(srf_data)) {
    rmd_content <- paste0(rmd_content, '

  # Reading and Visualizing SRF data
It expects a path to a directory as an input as well as a specification of the camera type.
The UAVsrf.plot function show the visualization of the spectral response function:
```{r}
UAVsrf.plot(srf_data, Cam_type, xmin = 350, xmax = 1000)

```
')
  }

  if (!is.null(UAV_data)) {
    rmd_content <- paste0(rmd_content, '
# Thresholds

The following image correction function expects the user to specify thresholds for the creation of NDVI and BVIS masks, the thresholds function serves as a visual guide for the user to select these thresholds.

```{r}
thresholds(x=UAV_data_clip)
```
')
  }

  if (!is.null(processing_output)) {
    rmd_content <- paste0(rmd_content, '

# Main processing and image correction

The function pp.UAVprocessing function has multiple outputs that are described in the function help. These outputs are always in the same order and are left blank if the according dataset was not created e.g. the NDVI raster is not created if the vegetation correction is not chosen. Choosing all masks as well as the correction the function looks as follows:
```{r}
img_data_clip_cor <- processing_output[[1]]
NDVI_mask <- processing_output[[2]]
BVIS_mask <- processing_output[[3]]
asd_cam <- processing_output[[4]]
cam_sample_pts <- processing_output[[5]]
corimg_samp_pts <- processing_output[[6]]


#plot NDVI and BVIS masks if they were created in the imageprocessing function
par(mfrow=c(1,2), mar=c(0, 0, 1, 0))
if(!is.null(NDVI_mask)){
  raster::plot(NDVI_mask, axes=FALSE, main="NDVI Mask", legend=FALSE, col=grey(1:100/100))
}

if(!is.null(BVIS_mask)){
  plot(BVIS_mask, axes=FALSE, main="BVIS Mask", legend=FALSE, col=grey(1:100/100))
}

#plot original and corrected image
raster::plot(UAV_data_clip, main="Image before correction")
raster::plot(img_data_clip_cor, main="Image after correction")

```
')
  }

  if (!is.null(predrespDS )) {
    rmd_content <- paste0(rmd_content, '

# Predictor Response Dataset


The function compilePR.UAV includes functionalities for generating a predictor response dataset. The values are calculated for each sampling point within a user specified radius. The resulting table shows the band values per sample point as well as the number of observations (pixels) taken into account for the calculation of the response datasets within the given radius.

```{r}
pred_resp_mean_cor <- predrespDS[[1]]
pred_resp_median_cor <- predrespDS[[2]]

head(pred_resp_mean_cor@data)
head(pred_resp_median_cor@data)

```
')
  }


rmd_content <- paste0(rmd_content, '
# Conclusion

This report summarizes the results obtained from using the SOCmonit package for UAV data analysis .
')

  # Write the content to the Rmd file
  cat(rmd_content, file = rmd_path)

  # Convert the Rmd file to a PDF using rmarkdown
  rmarkdown::render(rmd_path, output_file = pdf_path)

  # Remove the Rmd file
  file.remove(rmd_path)

  return(pdf_path)
}
