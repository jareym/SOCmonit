#' Build Predictor Response Dataset
#'
#' Method includes functions to generate a predictor response dataset from the original or corrected image.
#'
#' @param x RasterBrick. The image loaded using loadimgdata()
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam.
#' @param path1 character. The file path to the shapefile containing the sampling points with a column named "SAMPLE_ID" specifying the sample location names.
#' @param buffering logical. If TRUE the buffered plot shapefile is given in the next parameter, if FALSE the non buffered plots are expected
#' @param path2 character. The file path to the shapefile containing either the buffered or non buffered plots. This shapefile needs to contain a column named "PLOT_ID".
#' @param radius float. Defined the radius around sampling points for value extraction from the image. Define in meters or, if existing, or if existing in other map unit. Must be larger than pixel size.
#' @return Output:
#' Predictor response dataset mean [[1]]
#' Predictor response dataset median [[2]]
#' @examples
#' #create predictor response datasets considering buffered plots
#' predrespDS <- compilePR.UAV(img_data_clip_cor, cam_type = "Mica", SPpath = "path/tofile.shp",
#' buffering = TRUE, plotshp_path = "path/BL_V120_crops19_20_buffer.shp", radius= 3)
#'
#' pred_resp_mean_cor <- predrespDS[[1]]
#' pred_resp_median_cor <- predrespDS[[2]]
#'
#'
#' #create predictor response datasets without considering buffered plots
#' predrespDS <- compilePR.UAV(img_data_clip_cor, cam_type = "Mica", SPpath = "path/SP_(A)_V120_join.shp",
#' buffering = FALSE, plotshp_path = "path/BL_V120_crops19_20_buffer.shp", radius= 3)
#'
#' pred_resp_mean_cor <- predrespDS[[1]]
#' pred_resp_median_cor <- predrespDS[[2]]
#'
#' @export


compilePR.UAV <- function(x, cam_type, path1, buffering, path2, radius) {

  ###### Step 05: Predictor response dataset
  #Load sampling points
  sampling_pts <- raster::shapefile(path1)
  #rename the columns
  colnames(sampling_pts@data)[1] = "xcoord"
  colnames(sampling_pts@data)[2] = "ycoord"
  colnames(sampling_pts@data)[3] = "ID.plot"
  colnames(sampling_pts@data)[4] = "ID.point"

  #load second AOI with buffer used for second clip
  aoi_clip <- raster::shapefile(path2)
  colnames(aoi_clip@data)[1] = "ID.plot"

  if(buffering==TRUE){# clip image to plots
    x <- raster::crop(x, aoi_clip)
    x <- raster::mask(x,aoi_clip)
  }

  #use same CRS for points as for aoi
  sampling_pts <- sp::spTransform(sampling_pts, CRS(aoi_clip@proj4string@projargs))

  #generate dataframe that matches points in "sampling_points" with overlaying polygon
  point_in_plot_df <- sp::over(sampling_pts, aoi_clip)

  #create dataframe for mean values extracted at points
  img_cor_sample_pts_mean <- data.frame()
  #create dataframe for median values extracted at points
  img_cor_sample_pts_median <- data.frame()

  for(i in 1:nrow(sampling_pts)) {
    #get first point in sampling points
    p <- sampling_pts[i,]
    #get plot id at sampling point
    plot_id <- point_in_plot_df$ID.plot[i]
    #subset plot with plot if from complete shp with all plots
    clip_plot <- aoi_clip[aoi_clip$ID.plot == plot_id,]
    #clip image data with individual plot
    img_data_plotid_clip <- raster::crop(x, clip_plot)
    img_data_plotid_clip <- raster::mask(img_data_plotid_clip, clip_plot)

    #get the number of valid pixels used for mean and median calculation
    observations_na <- (raster::extract(img_data_plotid_clip[[1]], p, buffer = as.numeric(radius), na.rm = T))[[1]]
    observations <- observations_na[!is.na(observations_na)]
    numobs <- length(observations)

    #get mean of all values within radius around sampling point for each band
    sample_point_mean <- as.data.frame(raster::extract(img_data_plotid_clip, p, buffer = as.numeric(radius), fun = mean, na.rm = T))
    sample_point_mean$numobs <- numobs
    #write values for point in dataframe that will later contain all values from all points
    img_cor_sample_pts_mean <- rbind(img_cor_sample_pts_mean, sample_point_mean)

    sample_point_median <- as.data.frame(raster::extract(img_data_plotid_clip, p, buffer = as.numeric(radius), fun = median, na.rm = T))
    sample_point_median$numobs <- numobs
    img_cor_sample_pts_median <- rbind(img_cor_sample_pts_median, sample_point_median)


  }

  #add point ID to dataframe (table holding output statistics)
  img_cor_sample_pts_mean <- cbind(POINT = sampling_pts$ID.point, img_cor_sample_pts_mean)
  img_cor_sample_pts_median <- cbind(POINT = sampling_pts$ID.point, img_cor_sample_pts_median)


  #order both dataframes by sample location column and compare
  asd_cam <- asd_cam[order(asd_cam$ID.point, decreasing=F),]
  img_cor_sample_pts_mean <- img_cor_sample_pts_mean[order(img_cor_sample_pts_mean$POINT, decreasing=F),]
  img_cor_sample_pts_median <- img_cor_sample_pts_median[order(img_cor_sample_pts_median$POINT, decreasing=F),]
  #throw error if order is not the same
  #Making sure that only locations with both pieces of information is used in the next steps
  if(all(asd_cam$ID.point == img_cor_sample_pts_mean$ID.point) == FALSE) { stop("Spectral data does not match sampling point data.")}
  if(all(asd_cam$ID.point == img_cor_sample_pts_median$ID.point) == FALSE) { stop("Spectral data does not match sampling point data.")}


  #list with mean and median tables
  mean_and_median <- list(img_cor_sample_pts_mean, img_cor_sample_pts_median)

  pred_response_list <- list()

  for (j in 1:length(mean_and_median)){
    dataset <- mean_and_median[[j]]
    names = c("mean", "median")
    if (cam_type == "Tetra"){
      colnames(dataset) <-c("IDpoint", "b490","b570","b630","b720","b830", "numobs")
    } else if (cam_type == "Mica"){
      colnames(dataset) <-c("IDpoint", "b1","b2","b3","b5","b4","numobs")
    }
    shape <- SpatialPoints(sampling_pts)
    #create dataset with points
    sampling_pts_dataset <- sp::SpatialPointsDataFrame(shape, dataset)
    raster::crs(sampling_pts_dataset) <- raster::crs(sampling_pts)
    pred_response_list[[j]] <- sampling_pts_dataset
  }

  pred_resp_mean <- pred_response_list[[1]]
  pred_resp_median <- pred_response_list[[2]]

  outputlist <- list(pred_resp_mean, pred_resp_median)


  return(outputlist)
}
