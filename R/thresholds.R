#' NDVI and BVIS thresholds
#'
#' Method to generate NDVI and BVIS indices and determine threshold values used for creating masks.
#'
#' @return Function returns a scatter plot of
#' NDVI and BVIS as well as individual historgrams for both.
#'
#' @param x RasterBrick. The image loaded using loadimgdata()
#'
#'
#' @examples
#' #plot values for NDVI and BVIS calculated from input image
#' thresholds(x=UAV_img_clipped)
#'
#' @export

thresholds <- function(x) {
  # Check if the input is a RasterBrick object
  if (!inherits(x, "RasterBrick")) {
    stop("Input 'x' must be a RasterBrick object.")
  }

  # calculate NDVI using the red (band 3) and nir (band 5) bands
  img_NDVI <- (raster::values(x[[5]]) - raster::values(x[[3]])) / (raster::values(x[[5]]) + raster::values(x[[3]]))

  # calculate brightness VIS (band 1 + band 2 + band 3)
  img_BVIS <- raster::values(x[[1]]) + raster::values(x[[2]]) + raster::values(x[[3]])

  # make scatterplot from NDVI and brightness VIS layer to help user decide on the theshold values
  plot(img_NDVI, img_BVIS, xlab = "NDVI", ylab = "Brightness VIS", xaxt = "n", yaxt = "n")
  axis(1, at = seq(-0.3, 0.8, 0.01), labels = seq(-0.3, 0.8, 0.01), lwd = 0, lwd.ticks = 1.5, las = 2)
  axis(2, at = seq(-0.3, 0.8, 0.01), labels = seq(-0.3, 0.8, 0.01), lwd = 0, lwd.ticks = 1.5, las = 2)

  # Histogram NDVI
  raster::hist(img_NDVI, breaks = 50, freq = FALSE, prob = TRUE, main = "NDVI Histogram", xaxt = "n", xlab = "NDVI")
  axis(1, at = seq(-0.3, 0.8, 0.01), labels = seq(-0.3, 0.8, 0.01), lwd = 0, lwd.ticks = 1.5, las = 2)

  # Histogram BVIS
  raster::hist(img_BVIS, breaks = 50, freq = FALSE, prob = TRUE, main = "BVIS Histogram", xaxt = "n", xlab = "BVIS")
  axis(1, at = seq(-0.3, 0.8, 0.01), labels = seq(-0.3, 0.8, 0.01), lwd = 0, lwd.ticks = 1.5, las = 2)
}
