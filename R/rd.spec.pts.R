#' Read and correct spectral data
#'
#' This function reads spectral data from a file and performs optional corrections on the data.
#' @param file File location of the spectral data.
#' @param type Input format of the file, either "csv" or "text".
#' @param spec.columns Indices or names of the columns containing the spectral data.
#' @param ID.sample Name or index of the column representing the sample ID. If NULL, the sample ID will not be included in the output.
#' @param ID.point Name or index of the column representing the point ID (location ID).
#' @param asd.corr Logical value indicating whether to perform ASD data correction. If TRUE, the function applies correction calculations to the ASD data.
#' @param ... Additional arguments to be passed to the underlying functions such as read.csv and read.table.
#'
#' @return A data frame containing the processed spectral data. The data frame includes the specified columns, along with the optional sample and point ID columns.
#' @examples
#' # Read spectral data from a CSV file with sample and point IDs
#' data <- rd.spec.pts("spectra.csv", type = "csv", ID.point = "PointID", ID.sample = "SampleID", spec.columns = c(2:10), asd.corr = FALSE)
#' @export



rd.spec.pts<-function (file, type=c("csv", "text"), ID.point,
                        ID.sample=NULL, spec.columns, asd.corr = F, ...) {



  if (type=="csv") {
    data<-read.csv(file, ...)


  }

  if (type=="text") {
    data<-read.table(file, ...)

  }

   spectra <- data[, spec.columns]
    if (asd.corr == T) {
      asd_data_cor <- spectra
      for (i in 1:nrow(data)) {
        diff_sen12 <- asd_data_cor[i, 652] - asd_data_cor[i,
                                                          651]
        asd_data_cor[i, c(652:1451)] <- asd_data_cor[i, c(652:1451)] -
          diff_sen12
        diff_sen23 <- asd_data_cor[i, 1452] - asd_data_cor[i,
                                                           1451]
        asd_data_cor[i, c(1452:2151)] <- asd_data_cor[i,
                                                      c(1452:2151)] - diff_sen23
      }
     spectra<- asd_data_cor
    }
    if (is.null(ID.sample)) {
      ID.point<-subset(data, select=ID.point)
      out.spec <- data.frame(ID.point, spectra)
    }
    else {
      ID.point<-subset(data, select=ID.point)
      ID.sample<-subset(data, select=ID.sample)
      out.spec <- data.frame(ID.sample,ID.point, spectra)
      colnames(out.spec)[1:2]<-c("ID.sample", "ID.point")
       }
    out.spec
}
