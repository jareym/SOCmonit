#' Read ASD data and create dataframe
#'
#' Reads the ASD from individual .txt files into a dataframe and returns the original and corrected ASD dataframes as a list.
#'
#' @param path character. The path to the directory where the ASD data (without header) is saved
#' @param year character. The year that the ASD data was recorded. Currently only 2018 and 2019 are supported.
#' @param jumpcorr logial. If TRUE (default), sensor jump correction is conducted.
#' @param corrmethod character. Can be difference ("diff" - default) or multiplicative ("multi") approach.
#'
#' @return Output: If jumpcorr=F: Dataframe with original ASD data. If jumpcorr=T: List of two dataframes. ASD data before [[1]] and after [[2]] sensor jump correction.
#' 
#' @examples 
#' #read ASD data and conduct sensor jump correction
#' as_data <- rd.ASD(path="Path/todirectory", year="2018", jumpcorr=TRUE, corrmethod="diff")
#' #asd data without correction
#' asd_data_orig <- asd_data[[1]]
#' #asd data with correction
#' asd_data_cor <- asd_data[[2]]
#' 
#' #read ASD dara without sensor jump correction
#' as_data <- rd.ASD(path="Path/todirectory", year="2018", jumpcorr=FALSE)
#' 
#' @importFrom utils read.table read.delim
#'
#' @export


rd.ASD <- function(path, year, jumpcorr=TRUE, corrmethod = "diff"){


  # ASD data (without header) which are saved locally
  asd_files <- list.files(path, full.names = T)
  asd_names <- list.files(path)

  asd_data <- data.frame(ID.point = rep(NA, length(asd_names)), ID.sample = rep(NA, length(asd_names)))
  for(i in 350:2500){
    wl <- data.frame(name = rep(NA, length(asd_names)))
    names(wl) <- i
    asd_data <- cbind(asd_data, wl)
  }

  if ( year == 2018){
    for(i in 1:length(asd_names)){
      data <- read.table(asd_files[i], header = T) # format for 2018 data
      asd_data[i,1] <- toupper(substr(asd_names[i], 1, 4))
      asd_data[i,2] <- substr(asd_names[i], 6, 10)
      asd_data[i,c(3:2153)] <- data[,2]
    }
  } else if (year == 2019){
    for(i in 1:length(asd_names)){
      data <- read.delim(asd_files[i]) # format for 2019 data
      asd_data[i,1] <- toupper(substr(asd_names[i], 1, 3))
      asd_data[i,2] <- substr(asd_names[i], 7, 8)
      asd_data[i,c(3:2153)] <- data[,2]
    }
  }

  if (toupper(jumpcorr)==TRUE){
    # Sensor jump correction (ASD)
    # based on https://www.geo.tuwien.ac.at/downloads/wd/report/dorigo06_astools_help.pdf

    # use additive approach, unless this produces negative values
    # otherwise use multiplicative approach (e.g. wl_1001 / wl_1000)
    asd_data_cor <- asd_data

    if (tolower(corrmethod)=="diff"){
      for(i in 1:length(asd_names)){
        diff_sen12 <- asd_data_cor[i,654] - asd_data_cor[i,653] # wl_1001 - wl_1000
        asd_data_cor[i,c(654:1453)] <- asd_data_cor[i,c(654:1453)] - diff_sen12

        diff_sen23 <- asd_data_cor[i,1454] - asd_data_cor[i,1453] # wl_1801 - wl_1800
        asd_data_cor[i,c(1454:2153)] <- asd_data_cor[i,c(1454:2153)] - diff_sen23
      }
    } else if (tolower(corrmethod)=="multi") {
      for(i in 1:length(asd_names)){
        multi_sen12 <- asd_data_cor[i,654] / asd_data_cor[i,653] # wl_1001 / wl_1000
        asd_data_cor[i,c(654:1453)] <- asd_data_cor[i,c(654:1453)] / multi_sen12

        multi_sen23 <- asd_data_cor[i,1454] / asd_data_cor[i,1453] # wl_1801 / wl_1800
        asd_data_cor[i,c(1454:2153)] <- asd_data_cor[i,c(1454:2153)] / multi_sen23
      }
    }



    #uncorrected and corrected data are needed in output to be used as input for the plot function
    outputlist <- list(asd_data, asd_data_cor)

    return(outputlist)
  } else {
    #if sensor jump correction is not conducted then just return the loaded ASD data
    return(asd_data)
  }


}
