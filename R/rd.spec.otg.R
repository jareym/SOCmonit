#' Read on-the-go spectra data
#'
#' Read on-the-go spectral data from a file
#' @param file A character string of the file location.
#' @param type A character string specifying the file type, defaults to csv.
#' @param xcoord column name containing x axis coordinates
#' @param ycoord column name containing y axis coordinates
#' @param spec.columns define column range of spectral data
#' @param projection define projection (CRS arguments)
#' @param non.duplicates logical. if TRUE will remove duplicates
#' @param to.UTM logical. if TRUE will transform to UTM
#' @param UTM.proy add CRS argument for UTM coordinates
#' @param ... arguments to be passed to \link[utils]{read.csv} and \link[utils]{read.table}
#' @export
#' @import ggplot2
#' @import sp
#' @import mapproj
#' @details Allows to define coordinates system and transform degrees to UTM
#' @return An object of SpatialPointsDataFrame and a plot with point locations
#' @examples
#' #read a csv file
#' out.linespec<- rd.spec.otg(system.file("extdata/spec.onthego.csv", package="SOCmonit"),
#' type="csv",xcoord="Long",ycoord="Lat",spec.columns = 20:400, non.duplicates = F)
#'
#' #remove duplicates
#' out.linespec<- rd.spec.otg(system.file("extdata/spec.onthego.csv", package="SOCmonit"),
#'                          type="csv",xcoord="Long",ycoord="Lat",
#'                           spec.columns = 20:400, non.duplicates =T)
#'#convert Lat/Long to UTM
#' out.linespec<- rd.spec.otg(system.file("extdata/spec.onthego.csv", package="SOCmonit"),
#'                          type="csv",xcoord="Long",ycoord="Lat", spec.columns = 20:400, non.duplicates =T,
#'                          projection=("+proj=longlat +datum=WGS84"), to.UTM = T, UTM.proy="+init=epsg:25833")
#'
rd.spec.otg <- function (file, type = c("csv", "text"), xcoord, ycoord, spec.columns = NULL,
                         projection = NULL, non.duplicates = FALSE, to.UTM = FALSE,
                         UTM.proy = NULL, ...) {

  if (type == "csv") {
    data <- read.csv(file, ...)
  }

  if (type == "text") {
    data <- read.table(file, ...)
  }

  ID.otg <- 1:nrow(data)
  points <- data.frame(data[xcoord], data[ycoord])
  colnames(points) <- c("xcoord", "ycoord")
  sp::coordinates(points) <- ~xcoord + ycoord

  if (is.null(spec.columns)) {
    points <- sp::SpatialPointsDataFrame(points, data.frame(ID.otg,data))
  } else {
    points <- sp::SpatialPointsDataFrame(points, data.frame(ID.otg, data[, spec.columns]))
  }

  if (!is.null(projection)) {
    crs <- projection
    sp::proj4string(points) <- sp::CRS(crs)
  }

  if (!isFALSE(non.duplicates)) {
    z <- sp::zerodist(points)
    print(paste(nrow(z), "duplicates removed"))
    sp::remove.duplicates(points)
  }

  if (!isFALSE(to.UTM) && !is.null(UTM.proy)) {
    crs2 <- UTM.proy
    points <- sp::spTransform(points, sp::CRS(crs2))
  }
  pd<-data.frame(points)
  p <- ggplot2::ggplot(pd)  + ggplot2::geom_point(ggplot2::aes(xcoord, ycoord)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                   panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank()) +
    ggplot2::coord_fixed() + ggplot2::xlab("xcoord") + ggplot2::ylab("ycoord")


  print(p)

  out.linespec <- points
  return(out.linespec)
}

