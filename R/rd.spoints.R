#' Read and visualize Sampling Points
#'
#' Read sampling points coordinates from a file and visualize them on a scatter plot.
#'
#' @param file File location of the data.
#' @param type File type: "csv" for CSV files, "text" for text files.
#' @param xcoord Column name of the x-axis coordinates.
#' @param ycoord Column name of the y-axis coordinates.
#' @param ID.point Column name of the point ID (optional).
#' @param projection Projection information (CRS arguments) for spatial data (optional).
#' @param ... Additional arguments to be passed to read.csv and read.table functions.
#'
#' @return A SpatialPointsDataFrame object representing the sampling points.
#'
#' @import ggplot2
#' @import sp
#'
#' @examples
#' # Read sampling points from a CSV file and plot them
#' rd.spoints("points.csv", type = "csv", xcoord = "longitude", ycoord = "latitude", ID.point = "ID", projection = "+proj=longlat +datum=WGS84")
#'
#' @export
rd.spoints <- function(file, type = c("csv", "text"), xcoord, ycoord,
                       ID.point = NULL, projection = NULL, ...) {

  if (type == "csv") {
    data <- read.csv(file, ...)
  } else if (type == "text") {
    data <- read.table(file, ...)
  }

  if (!xcoord %in% colnames(data) || !ycoord %in% colnames(data)) {
    stop("The specified columns for xcoord and ycoord do not exist in the data.")
  }

  if (is.null(ID.point)) {
    ID.point <- 1:nrow(data)
  }

  points <- data.frame(data[xcoord], data[ycoord])
  colnames(points) <- c("xcoord", "ycoord")
  sp_points <- sp::SpatialPoints(points)
  points_df <- data.frame(ID.point = data[[ID.point]], xcoord = data[[xcoord]], ycoord = data[[ycoord]])
  sp_points_df <- sp::SpatialPointsDataFrame(sp_points, points_df)

  if (!is.null(projection)) {
    crs <- projection
    sp::proj4string(sp_points_df) <- sp::CRS(crs)
  }

  pd <- data.frame(sp_points_df@data)
  p <- ggplot2::ggplot(pd) +
    ggplot2::geom_point(ggplot2::aes(x = xcoord, y = ycoord)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed() +
    ggplot2::xlab("xcoord") +
    ggplot2::ylab("ycoord")

  print(p)
  sp_points_df
}
