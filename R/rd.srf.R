#' Load SRF data from Excel file and create dataframe
#'
#' Loads the SRF data from an Excel file and creates a dataframe. The final shape of the dataframe is the same for
#' both camera systems but requires different processing steps.
#' @param path character. The path to the directory where the SRF file(s) is saved. Format Tetra: 490FS10-25 92778 X242-241 59116 23^C 0^A RANDOM^POL_1.csv, Format Mica: RedEdge_3_Filters_srs-3.ods. 
#' @param cam_type character. The type of camera the image was recorded with. Can be "Mica" for Micasense or "Tetra" for Tetracam
#' 
#' @examples 
#' #read spectral response functions for Micasense
#' srf_mica <- rd.srf(path="Path/todirectory", cam_type="Mica")
#' 
#' #' #read spectral response functions for Tetracam
#' srf_tetra <- rd.srf(path="Path/todirectory", cam_type="Tetra")
#' 
#' @import utils
#' @import readODS
#'
#' @export



rd.srf <- function(path, cam_type){


  if (cam_type == "Tetra"){

    col_names <- c("Wavelength_nm", "Transmission_per")
    data_clean <- function(input, col_names){
      input <- input[-c(1:6),]
      names(input) <- col_names
      len <- length(input[,1])
      input[,1] <- as.numeric(paste(input[,1]))
      input[,2] <- as.numeric(paste(input[,2]))

      input
    }

    srf <- data.frame(wl = seq(350, 2500, by = 0.1), b830 = rep(0, 21501),
                      b490 = rep(0, 21501), b570 = rep(0, 21501), b630 = rep(0, 21501),
                      b720 = rep(0, 21501))

    b830_init <- read.csv(paste0(path,"\\830FS10-25 92778 X242-22 56803 23^C 0^A RANDOM^POL_1.csv"))
    b830 <- data_clean(b830_init, col_names)
    start <- which(b830$Wavelength_nm[1] == srf$wl)
    end <- which(tail(b830$Wavelength_nm, n = 1) == srf$wl)
    srf$b830[c(start:end)] <- b830$Transmission_per

    b490_init <- read.csv(paste0(path,"\\490FS10-25 92778 X242-241 59116 23^C 0^A RANDOM^POL_1.csv"))
    b490 <- data_clean(b490_init, col_names)
    start <- which(b490$Wavelength_nm[1] == srf$wl)
    end <- which(tail(b490$Wavelength_nm, n = 1) == srf$wl)
    srf$b490[c(start:end)] <- b490$Transmission_per

    b570_init <- read.csv(paste0(path,"/570FS10-25 92778 X242-86 55863 23^C 0^A RANDOM^POL_1.csv"))
    b570 <- data_clean(b570_init, col_names)
    start <- which(b570$Wavelength_nm[1] == srf$wl)
    end <- which(tail(b570$Wavelength_nm, n = 1) == srf$wl)
    srf$b570[c(start:end)] <- b570$Transmission_per

    b630_init <- read.csv(paste0(path,"/630FS10-25 92778 X242-82 55901 23^C 0^A RANDOM^POL_1.csv"))
    b630 <- data_clean(b630_init, col_names)
    start <- which(b630$Wavelength_nm[1] == srf$wl)
    end <- which(tail(b630$Wavelength_nm, n = 1) == srf$wl)
    srf$b630[c(start:end)] <- b630$Transmission_per

    b720_init <- read.csv(paste0(path,"/720FS10-25 86977 V149-83 52953 23^C 0^A RANDOM^POL.csv"))
    b720 <- data_clean(b720_init, col_names)
    start <- which(b720$Wavelength_nm[1] == srf$wl)
    end <- which(tail(b720$Wavelength_nm, n = 1) == srf$wl)
    srf$b720[c(start:end)] <- b720$Transmission_per
  } else if (cam_type == "Mica") {
    srf_dir_mica <- paste0(path,"/RedEdge_3_Filters_srs-3.ods")

    #create srf dataframe
    srf <- data.frame(wl = seq(350, 2500, by = 0.1), b4 = rep(0, 21501),
                      b1 = rep(0, 21501), b2 = rep(0, 21501), b3 = rep(0, 21501),
                      b5 = rep(0, 21501))

    #read micasense srf data from file
    mica_data <- read_ods(path = srf_dir_mica, sheet = 2, col_names = FALSE, skip = 1)
    #replace column names
    colnames(mica_data) <- c("wl", "b1", "b2", "b3", "b5", "b4")
    #reorder colums so they fit the tetracam srf data
    mica_data <- mica_data[,c(1,6,2,3,4,5)]
    mica_data[, 2:6] <- mica_data[, 2:6]*100
    #expand the number of rows so they fit the tetracam srf data
    mica_data_exp <- mica_data[rep(seq_len(nrow(mica_data)), each = 10), ]

    #insert micasense srf data into the srf dataframe of specified length and width to use in further processing
    start <- which(mica_data_exp$wl[1] == srf$wl)
    end <- which(tail(mica_data_exp$wl, n = 1) == srf$wl)
    srf[start:(end+9), 2:6] <- mica_data_exp[, 2:6]
  }

  return(srf)
}
