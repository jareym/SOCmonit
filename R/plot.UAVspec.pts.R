#' Comparison plot
#'
#' Method to visualize the difference between the image values and the srf corrected ASD data that are used to correct the image values.
#'
#' @param x1 dataframe. The ASD data created using the loadASDdata function or in the specified format.
#' @param x2 dataframe. The dataframe holding spectral UAV data at the sampling points (output of the pp.UAVprocessing function)
#' @param x3 asd data after srf translation
#' @param ID.point character. The name of the sample location to be plotted (Default: "040A")
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam
#' @param xmin numeric. Minimum value displayed on the x-axis
#' @param xmax numeric. Maximum value displayed on the x-axis
#'
#' @return Output: Graph, boxplot for difference (asd_cam vs cam_sample_pts) and ratio (asd_cam vs cam_sample_pts)
#' @examples
#' #plot difference between the image values and the srf corrected ASD data
#' plot.UAVspec.pts(x1=asd_data_cor, x2=cam_sample_pts, x3=asd_cam, ID.point = "003A", cam_type = "mica", xmin = 350, xmax = 2000)
#'
#' @import graphics
#'
#' @export


UAVspec.pts.plot <- function(x1, x2, x3, ID.point = "040A", cam_type, xmin, xmax){

  # compare individual plots
  plot <- ID.point
  k <- 21
  num <- which(x1$ID.point == plot)
  curves <- data.frame(median = rep(NA, 2151))
  for(i in 1:2151){
    curves$median[i] <- median(x1[num,i+2])
  }

  plot(seq(350, 2500, by = 1), curves$median, type = "l", ylim = c(0,1), xlim = c(xmin,xmax),
       xlab = "wavelength (nm)", ylab = "reflectance (%)", main="ASD Data Comparison")
  legend("bottomright", legend=c("image spectra corrected", "image spectra original"), cex=0.6, pch=19, bty = "n", col= c("black", 2))

  if (tolower(cam_type) == "mica"){
    points(c(475,560,668,717,840), x2[k,1:5], pch = 19, col = 2)
    lines(c(475,560,668,717,840), x3[k,c(3:6,2)], lwd = 5)
  } else if (tolower(cam_type) == "tetra"){
    points(c(490,570,630,720,830), x2[k,1:5], pch = 19, col = 2)
    lines(c(490,570,630,720,830), x3[k,c(3:6,2)], lwd = 5)
  } else {
    print("The camera type you specified is not supported. Please check the spelling or use a different dataset.")
  }

  #Statistics used for Image correction
  # difference statistics
  diff <- x3[,c(3:6,2)] - x2[,1:5]
  graphics::boxplot(diff, main = "Difference Statistics", xaxt="n")
  axis(1, at = c(1,2,3,4,5), labels = c("b1", "b2", "b3", "b4", "b5"))
  # difference variation increases with wavelength

  # ratio statistics
  ratio <- x2[,1:5]/x3[,c(3:6,2)]
  graphics::boxplot(ratio, main = "Ratio Statistics", xaxt="n")
  axis(1, at = c(1,2,3,4,5), labels = c("b1", "b2", "b3", "b4", "b5"))
  # ratio variation more constant



}
