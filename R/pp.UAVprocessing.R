#' Main processing
#'
#' Method includes functions to mask the image using an NDVI and or BVIS mask generated using the given thresholds and conduct
#' image correction using an ASD correction function.
#'
#' @param x RasterBrick. The image loaded using loadimgdata()
#' @param mask_veg logical. If TRUE, vegetation disturbances will be masked using the NDVI and a threshold (NDVI_threshold)
#' @param mask_spec logical. If TRUE, spectral disturbances will be masked using the BVIS and quantiles.
#' @param NDVI_threshold float. Values larger than the threshsold will be masked out. Default = 0.74
#' @param upper_quant float. Values larger than the upper quantile will be masked out (BVIS). Default = 0.98
#' @param lower_quant float. Values lower than the lower quantile will be masked out (BVIS). Default = 0.02
#' @param correction logial. If TRUE, image correction using spectral (ASD) correction function will be conducted.
#' @param asd_data dataframe. The sensorjump corrected dataframe created using the loadASDdata function. Or alternatively a dataframe with the columns "ID.point", "ID.sample", "360".....(All wavelengths as numbers. Can be float or integer.).
#' @param srf dataframe. The dataframe created using the loadSRFdata function.
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam.
#' @param path character. The file path to the shapefile containing the sampling points. It should have 4 columns in this order: xcoord, ycoord, IDplot, IDpoint.
#' @param corr_method character. The type of correction method that should be used. Can be "ratio" or "diff".
#'
#' @return Output:
#' image (state of the image depends on the previous selection: masked, corrected..) [[1]]
#' NDVI mask [[2]]
#' BVIS mask [[3]]
#' SRF corrected ASD data [[4]]
#' image spectra at SP locations [[5]]
#' set of corrected image values at sampling point location [[6]]
#'
#' @examples
#' #mask image and conduct image correction using the specified parameters
#' processingoutput <- pp.UAVprocessing(x=img_data_clip, mask_veg = TRUE, mask_spec = TRUE, NDVI_threshold = 0.47,
#' upper_quant = 0.98, lower_quant = 0.02, correction = TRUE, asd_data = asd_data_cor, srf = srf_mica,
#' cam_type = "Mica", path = "Path/tofile.shp", corr_method = "ratio")
#'
#' #the output is a list of datastes. To get the individual datasets the elements have to be assigned to their own variable
#' img_data_clip_cor <- processingoutput[[1]]
#' NDVI_mask <- processingoutput[[2]]
#' BVIS_mask <- processingoutput[[3]]
#' asd_cam <- processingoutput[[4]]
#' cam_sample_pts <- processingoutput[[5]]
#' corimg_samp_pts <- processingoutput[[6]]
#'
#'
#' #correct image without previous masking
#' processingoutput <- pp.UAVprocessing(x=img_data_clip, mask_veg = FALSE, mask_spec = FALSE, correction = TRUE, asd_data = asd_data_cor, srf = srf_mica,
#' cam_type = "Tetra", path = "Path/tofile.shp", corr_method = "diff")
#'
#'
#' @export


pp.UAVprocessing <- function(x, mask_veg, mask_spec, NDVI_threshold = 0.47, upper_quant = 0.98, lower_quant = 0.02,

                             correction, asd_data, srf, cam_type,

                             path, corr_method

){


  #Define parameters as NULL in order to avoid error in return() functions when arguments are not calculated
  NDVI_mask <- NULL
  BVIS_mask <- NULL
  corimg_samp_pts <- NULL


  ###### Step 02: Calculate NDVI and apply threshold to mask green vegetation
  if (toupper(mask_veg)==TRUE){
    # calculate NDVI using the red (band 3) and nir (band 5) bands
    img_NDVI <- (x[[5]] - x[[3]]) / (x[[5]] + x[[3]])

    #create NDVI mask
    NDVI_mask <- img_NDVI
    NDVI_mask[NDVI_mask > as.numeric(NDVI_threshold)] <- NA
    NDVI_mask[NDVI_mask <= as.numeric(NDVI_threshold)] <- 1

    #mask image with NDVI and BVIS masks
    x <- raster::mask(x, NDVI_mask)
  }

  ###### Step 03: Calculate Brightness of visible band (BVIS) indiex and apply thresholds to mask spectral disturbances
  if (toupper(mask_spec)==TRUE){
    # calculate brightness VIS (band 1 + band 2 + band 3)
    img_BVIS <- (x[[1]] + x[[2]] + x[[3]])

    #calculate quantiles
    quantiles <- raster::quantile(img_BVIS, probs = c(as.numeric(lower_quant),as.numeric(upper_quant)))
    #generate brightness VIS mask
    BVIS_mask <- img_BVIS
    BVIS_mask[BVIS_mask < quantiles[[1]] | BVIS_mask > quantiles[[2]]] <- NA
    BVIS_mask[BVIS_mask >= quantiles[[1]] & BVIS_mask <= quantiles[[2]]] <- 1

    #mask image with BVIS mask
    x <- raster::mask(x, BVIS_mask)
  }

  ###### Step interim: load data that is required in Step 4 as well as Step 5
  #Load sampling points here as they are always used no matter if correction is chosen or not (are used later in the script, Step 4.1 and/or Step 5)
  sampling_pts <- raster::shapefile(path)
  #rename the columns
  colnames(sampling_pts@data)[1] = "xcoord"
  colnames(sampling_pts@data)[2] = "ycoord"
  colnames(sampling_pts@data)[3] = "ID.plot"
  colnames(sampling_pts@data)[4] = "ID.point"

  ## Reprocess ASD data to Camera signal
  # take median of trials for sample location
  # changing it here from median to mean

  asd_data_mean <- data.frame(ID.point = unique(asd_data$ID.point))

  for(i in 3:ncol(asd_data)){
    wl_mean <- data.frame(data = tapply(asd_data[,i], asd_data$ID.point, mean))
    names(wl_mean) <- names(asd_data)[i]
    asd_data_mean <- cbind(asd_data_mean, wl_mean)
  }

  # SRF translation
  reps <- nrow(asd_data_mean)
  #reps <- nrow(asd_data_med)


  if (cam_type == "Tetra"){
    # SRF translation

    asd_cam <- data.frame(ID.point = unique(asd_data$ID.point), b830 = rep(NA, reps),
                          b490 = rep(NA, reps), b570 = rep(NA, reps), b630 = rep(NA, reps), b720 = rep(NA, reps))

  }else if (cam_type == "Mica") {
    asd_cam <- data.frame(ID.point = unique(asd_data$ID.point), b4 = rep(NA, reps),
                          b1 = rep(NA, reps), b2 = rep(NA, reps), b3 = rep(NA, reps), b5 = rep(NA, reps))

  }

  #get the wavelengths recorded by the spectormeter
  asd_wl = round(as.numeric(names(asd_data_mean[2:ncol(asd_data_mean)])), digits = 1)
  #build a subset of srf data that only consits of wavelengths also covered in the spectrometer data
  srf_sub = srf[srf$wl %in% asd_wl,]

  for(j in 1:nrow(asd_data_mean)){
    asd_srf_comp <- as.numeric(asd_data_mean[j,-1])
    for(i in 1:5){
      n <- length(asd_srf_comp)
      asd_cam[j,i+1] <- sum(srf_sub[,i+1]*asd_srf_comp[c(1:n)])/sum(srf_sub[,i+1])
    }
  }


  ###### Step 04: Apply image correction

  ###### Compare sampling site ASD signal to drone data (Step 4.1)
  #Extract spectra at sampling point locations

  radius1 <- 0.25 #in meters

  #create dataframe for mean values extracted at points
  cam_sample_pts <- data.frame()

  for(i in 1:nrow(sampling_pts)){
    #get first point in sampling points
    p <- sampling_pts[i,]

    #get number of valid pixels used for calculation of mean and subsequent image correction
    corrobs_na <- (raster::extract(x[[1]], p, buffer = as.numeric(radius1), na.rm = T))[[1]]
    corrobs <- corrobs_na[!is.na(corrobs_na)]
    numobs <- length(corrobs)

    # extract values from image data at sample locations with a buffer of 25cm
    cam_sample_pt <- as.data.frame(raster::extract(x, p, buffer = as.numeric(radius1), fun = mean, na.rm = T))
    cam_sample_pt$numobs <- numobs
    cam_sample_pt$ID.point <- p$ID.point


    cam_sample_pts <- rbind(cam_sample_pts, cam_sample_pt)
  }


  #order both dataframes by sample location column and compare
  asd_cam <- asd_cam[order(asd_cam$ID.point, decreasing=F),]
  cam_sample_pts <- cam_sample_pts[order(cam_sample_pts$ID.point, decreasing=F),]
  #throw error if order is not the same
  #Step 2 of making sure that only locations with both pieces of information is used in the data correction step
  if(all(asd_cam$ID.point == cam_sample_pts$ID.point) == FALSE) { stop("Spectral data does not match sampling point data.")}


  if (toupper(correction)==TRUE){
    ###### Build spectral (ASD) correction functions (Step 4.2)

    # difference statistics
    diff <- asd_cam[,c(3:6,2)] - cam_sample_pts[,1:5]
    #boxplot(diff, main = "Difference Statistics")
    # difference variation increases with wavelength

    # ratio statistics
    ratio <- cam_sample_pts[,1:5]/asd_cam[,c(3:6,2)]
    colnames(ratio)<- colnames(asd_cam[,c(3:6,2)])
    #boxplot(ratio, main = "Ratio Statistics")
    # ratio variation more constant


    ###### Apply image correction (step 4.3)

    if (tolower(corr_method=="diff")){
      if (cam_type == "Tetra"){
        img_data_clip_cor <- raster::brick(x[[1]]/median(diff$b490, na.rm = TRUE),
                                   x[[2]]/median(diff$b570, na.rm = TRUE), x[[3]]/median(diff$b630, na.rm = TRUE),
                                   x[[4]]/median(diff$b720, na.rm = TRUE), x[[5]]/median(diff$b830, na.rm = TRUE))
      } else if (cam_type == "Mica"){
        img_data_clip_cor <- raster::brick(x[[1]]/median(diff$b1, na.rm = TRUE),
                                   x[[2]]/median(diff$b2, na.rm = TRUE), x[[3]]/median(diff$b3, na.rm = TRUE),
                                   x[[4]]/median(diff$b5, na.rm = TRUE), x[[5]]/median(diff$b4, na.rm = TRUE))
      }
    } else if (tolower(corr_method=="ratio")){
      if (cam_type == "Tetra"){
        img_data_clip_cor <- raster::brick(x[[1]]/median(ratio$b490, na.rm = TRUE),
                                   x[[2]]/median(ratio$b570, na.rm = TRUE), x[[3]]/median(ratio$b630, na.rm = TRUE),
                                   x[[4]]/median(ratio$b720, na.rm = TRUE), x[[5]]/median(ratio$b830, na.rm = TRUE))
      } else if (cam_type == "Mica"){
        img_data_clip_cor <- raster::brick(x[[1]]/median(ratio$b1, na.rm = TRUE),
                                   x[[2]]/median(ratio$b2, na.rm = TRUE), x[[3]]/median(ratio$b3, na.rm = TRUE),
                                   x[[4]]/median(ratio$b5, na.rm = TRUE), x[[5]]/median(ratio$b4, na.rm = TRUE))
      }
    }


    corimg_samp_pts <- raster::extract(img_data_clip_cor, sampling_pts)

    x <- img_data_clip_cor
  }


  outputlist <- list(x, NDVI_mask, BVIS_mask, asd_cam, cam_sample_pts, corimg_samp_pts)


  return(outputlist)

}
