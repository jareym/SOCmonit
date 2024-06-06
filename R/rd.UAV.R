#' Read image data and option for clipping
#'
#' Method to read an orthofoto and convert pixel values to reflectance based on specified camera type. The aoi is used to clip the
#' image to the respective plots used in the analysis.
#'
#' @param path character. The file path to the image that is to be loaded
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam
#' @param aoipath character. The file path to the aoi that is used to clip the image
#'
#' @return
#' Raster dataset as a whole or clipped.
#'
#' @examples
#' #read UAV image and clip to area of interest
#' img_data_clip <- rd.UAV(path="Path/tofile.tif", cam_type="Mica", aoipath="Path/tofile.shp")
#'
#'
#'
#' @export
#'


rd.UAV <- function(path, cam_type, aoipath){

  image_data_orig <- raster::brick(path)

  # Convert pixel values to reflectance
  if (tolower(cam_type) == "mica" & raster::dataType(image_data_orig)=="INT2U"){
    # translate 16-vit values to % reflectance
    img_data <- image_data_orig/32768
  } else if (tolower(cam_type) == "tetra"){
    # translate 10-bit values to % reflectance, same scale [0,1] as ASD data
    img_data <- image_data_orig/102300
  } else {
    print("The camera type you specified is not supported. Please check the spelling or use a different dataset.")
  }

  # load AOI
  aoi_clip <- raster::shapefile(aoipath)

  # clip image to plots
  img_data_clip <- raster::crop(img_data, aoi_clip)
  img_data_clip <- raster::mask(img_data_clip,aoi_clip)

  return(img_data_clip)
}
