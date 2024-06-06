#' Comparison plot
#'
#' Method to compare (corrected) ASD data, camera data and corrected camera data at all sample locations
#'
#' @param x1 dataframe. The ASD data created using the loadASDdata function or in the specified format.
#' @param x2 dataframe. The dataframe holding sample point data from the UAV data (output of the pp.UAVprocessing function)
#' @param x3 dataframe. asd data after srf translation
#' @param x4 dataframe. values extracted from the corrected image
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam
#' @param ID.point character. The name of the sample location to be plotted (Default: "005A")
#'
#' @examples
#' #plot comparison of corrected ASD data camera data and corrected camera data for each sample location
#' plot.UAV_UAV.ASDcor(x1=asd_data_cor, x2=cam_sample_pts, x3=asd_cam, x4=corimg_samp_pts, cam_type="Mica", ID.point="001A")
#'
#'
#' @export

UAV_UAV.ASDcor.plot <- function(x1, x2, x3, x4, cam_type, ID.point= "005A"){

  if(is.null(corimg_samp_pts)){
    print("Plot can only be created if corrected image values are supplied.")
  } else {
    num <- which(x1$ID.point == ID.point)
    k = which(x2$ID.point == ID.point)
    curves <- data.frame(median = rep(NA, 2151))
    for(i in 1:2151){
      curves$median[i] <- median(x1[num,i+2])
    }
    plot(seq(350, 2500, by = 1), curves$median, type = "l", ylim = c(0,1), xlim = c(475,840),
         xlab = "wavelength (nm)", ylab = "reflectance (%)", main=paste("Image correction example for", ID.point, sep=" "))
    legend("topright", legend=c("image spectra corrected", "image spectra original", "corrected ASD data"), pch = c(19, 19,NA), lty = c(NA, NA, 1), bty = "n", col= c(2, "gray", "black"))

    if (tolower(cam_type) == "mica"){
      points(c(475,560,668,717,840), x2[k,1:5], pch = 19, col = "gray")
      points(c(475,560,668,717,840), x4[k,], pch = 19, col = 2)
    } else if (tolower(cam_type) == "tetra"){
      points(c(490,570,630,720,830), x2[k,1:5], pch = 19, col = "gray")
      points(c(490,570,630,720,830), x4[k,], pch = 19, col = 2)
    } else {
      print("The camera type you specified is not supported. Please check the spelling or use a different dataset.")
    }

  }

}


