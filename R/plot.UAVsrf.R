#' SRF Plot
#'
#' Method to visualize spectral response functions (SRF) used in the further analysis.
#'
#' @param x SRF data generated using the loadSRFdata function
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam
#' @param xmin numeric. Minimum value displayed on the x-axis
#' @param xmax numeric. Maximum value displayed on the x-axis
#'
#' @examples 
#' #plot the loaded spectral response function data for MicaSense 
#' plot.UAVsrf(x=srf_mica, cam_type="Mica", xmin = 350, xmax = 1000)
#' 
#' #plot the loaded spectral response function data for Tetracam 
#' plot.UAVsrf(x=srf_tetra, cam_type="Tetra", xmin = 350, xmax = 1000)
#' 
#' @import wesanderson
#'
#' @export


UAVsrf.plot <- function(x, cam_type, xmin, xmax){


  #check SRF data
  pal <- wes_palette("Cavalcanti1")



  #check camera type as naming convention in dataframe is different depending on which srf dataset was loaded
  if (tolower(cam_type) == "mica"){
    plot(x$wl, rep(0, length(x$wl)), col = "white", xlab = "wavelength (nm)",
         ylab = "transmission (%)", ylim = c(0,90), xlim = c(xmin,xmax), main="SRF data - Micasense")
    polygon(x$wl, x$b1, col = pal[1], border = pal[1])
    polygon(x$wl, x$b2, col = pal[2], border = pal[2])
    polygon(x$wl, x$b3, col = pal[3], border = pal[3])
    polygon(x$wl, x$b5, col = pal[5], border = pal[5])
    polygon(x$wl, x$b4, col = pal[4], border = pal[4])
  } else if (tolower(cam_type) == "tetra"){
    plot(x$wl, rep(0, length(x$wl)), col = "white", xlab = "wavelength (nm)",
         ylab = "transmission (%)", ylim = c(0,90), xlim = c(xmin,xmax), main="SRF data - Tetracam")
    polygon(x$wl, x$b490, col = pal[1], border = pal[1])
    polygon(x$wl, x$b570, col = pal[2], border = pal[2])
    polygon(x$wl, x$b630, col = pal[3], border = pal[3])
    polygon(x$wl, x$b720, col = pal[5], border = pal[5])
    polygon(x$wl, x$b830, col = pal[4], border = pal[4])
  } else {
    print("The camera type you specified is not supported. Please check the spelling or use a different dataset.")
  }
}
