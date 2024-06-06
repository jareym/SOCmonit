#' Plot sensor jump correction
#'
#' Method to load an orthofoto and convert pixel values to reflectance based on specified camera type. The aoi is used to clip the
#' image to the respective plots used in the analysis.
#'
#' @param x1 dataframe. The dataframe created using the loadASDdata function. Needs a column named samp_loc.
#' @param x2 dataframe. The corrected dataframe created using the loadASDdata function. Needs a column named samp_loc.
#' @param ID.point character: The name of the sample location to be plotted (Default: "001A")
#'
#' @return Output: 5 Plots: Extracts from ASD data at sensor jump locations before (2 plots) and after (2 plots) the correction as well as 1 plot
#' showing the corrected ASD data at a sample location.
#'
#' @examples
#' #plot results of ASD sensor jump correction
#' plot.ASDjump(x1=asd_data_orig, x2=asd_data_cor, ID.point ="005A")
#'
#' @export


ASDjump.plot <- function(x1, x2, ID.point = "001a"){


  # Sensor jump correction (ASD)
  # based on https://www.geo.tuwien.ac.at/downloads/wd/report/dorigo06_astools_help.pdf
  base::plot(seq(640,660, by = 1), x1[1, c(643:663)], type = "l", main=("ASD data before sensor jump correction"), cex.main=0.8, xlab="wavelength (nm)", ylab="reflectance (%)"); asd_data_orig[1, c(643:663)]
  # detector 1: 350-1000 nm
  # detector 2: 1001 - 1800 nm
  base::plot(seq(1790,1810, by = 1), x1[1, c(1443:1463)], type = "l", main=("ASD data before sensor jump correction"), cex.main=0.8, xlab="wavelength (nm)", ylab="reflectance (%)"); asd_data_orig[1, c(1443:1463)]
  # detector 3: 1801 - 2500 nm

  base::plot(seq(640,660, by = 1), x2[1, c(643:663)], type = "l", main=("ASD data after sensor jump correction"), cex.main=0.8, xlab="wavelength (nm)", ylab="reflectance (%)"); asd_data_cor[1, c(643:663)]
  # detector 1: 350-1000 nm
  # detector 2: 1001 - 1800 nm
  base::plot(seq(1790,1810, by = 1), x2[1, c(1443:1463)], type = "l", main=("ASD data after sensor jump correction"), cex.main=0.8, xlab="wavelength (nm)", ylab="reflectance (%)"); asd_data_cor[1, c(1443:1463)]
  # detector 3: 1801 - 2500 nm

  # Plot sample location data
  plot <- ID.point

  num <- which(x2$ID.point == plot)

  # as percent curves
  curves <- data.frame(median = rep(NA, 2151), mean = rep(NA, 2151), upper_99 = rep(NA, 2151),
                       upper_68 = rep(NA, 2151), lower_34 = rep(NA, 2151), lower_1 = rep(NA, 2151))

  for(i in 1:2151){
    curves$median[i] <- median(x2[num,i+2])
    curves$mean[i] <- mean(x2[num,i+2])
    curves[i,c(6:3)] <- quantile(x2[num,i+2], probs = c(0.01, 0.34, 0.68, 0.99))
  }

  base::plot(seq(350, 2500, by = 1), x2[1,c(3:2153)], col = "white", ylim = c(0,1),
       xlab = "wavelength (nm)", ylab = "reflectance (%)", main="ASD data at sample location", cex.main=0.8)
  polygon(c(350, seq(350, 2500, by = 1), 2500), c(0.1,curves$upper_68,0.1), col = "gray", border = "gray")
  polygon(c(350, seq(350, 2500, by = 1), 2500), c(0.1,curves$lower_34,0.1), col = "white", border = "white")
  lines(seq(350, 2500, by = 1), curves$median)
  lines(seq(350, 2500, by = 1), curves$mean, lty = "dashed")
  lines(seq(350, 2500, by = 1), curves$upper_99, lty = "dotted")
  lines(seq(350, 2500, by = 1), curves$lower_1, lty = "dotted")
  text(2500, 0.2, plot)

}
