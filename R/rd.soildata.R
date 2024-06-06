#' Read soil data
#'
#' Read soil data from external files.
#'
#' @param file File location.
#' @param type File type indicator.
#'   - "csv" indicates that the file is in CSV  format.
#'   - "text" indicates that the file is in plain text format.
#' @param soc.values Column name of soc values.
#' @param ID.sample Column name of Sample ID.
#' @param ID.point Column name of point ID (location number).
#' @param average_by_point Logical indicating whether to average the data by ID.point.
#' @param ... Arguments to be passed to read.csv and read.table functions.
#'
#' @return A data frame containing the soil data.
#'
#' @examples
#' # Read soil data from a CSV file without averaging
#' out.socdata <- rd.soildata(system.file("extdata/socdata.csv",package="SOCmonit"), type = "csv", soc.values = "C",
#'                           ID.sample = "Code", ID.point = "ID", average_by_point = FALSE)
#'
#' # Read soil data from a CSV file with averaging
#' out.socdata_avg <- rd.soildata(system.file("extdata/socdata.csv",package="SOCmonit"), type = "csv", soc.values = "C",
#'                               ID.sample = NULL, ID.point = "ID", average_by_point = TRUE)
#'
#' @export
rd.soildata <- function(file, type = c("csv", "text"), soc.values,
                        ID.sample = NULL, ID.point, average_by_point = FALSE, ...) {

  if (type == "csv") {
    data <- read.csv(file, ...)
  } else if (type == "text") {
    data <- read.table(file, ...)
  }

  pointID <- data[[ID.point]]
  soc <- data[[soc.values]]

  if (is.null(ID.sample)) {
    sampleID <- seq_along(pointID)
    out.soildata <- data.frame(ID.sample = sampleID, ID.point = pointID, soc.values = soc)
  } else {
    sampleID <- data[[ID.sample]]
    out.soildata <- data.frame(ID.sample = sampleID, ID.point = pointID, soc.values = soc)
  }

  if (average_by_point) {
    avg_soc_values <- tapply(out.soildata$soc.values, out.soildata$ID.point, mean, na.rm = TRUE)
    out.soildata <- data.frame(ID.point = as.numeric(names(avg_soc_values)), soc.values = avg_soc_values)
  }

  out <- out.soildata
  return(out)
}
