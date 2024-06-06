#' Generate a LTE map
#'
#' Generate plot maps based on field vertices with optional rotation.
#'
#' @param x1-x4 x-axis coordinates of the field vertices
#' @param y1-y4 y-axis coordinates of the field vertices
#' @param col.plot number of columns in the plot map
#' @param row.plot number of rows in the plot map
#' @param data optional field data to include in the map
#'
#' @return A list with two elements:
#'   \item{map}{The plot map as a \code{SpatialPolygonsDataFrame} object.}
#'   \item{plot}{The plot of the map.}
#'
#' @import sp
#'
#' @examples
#'LTE.out <- map.LTE(x1 = 0, x2 = 5, x3 = 10, x4 = 15, y1 = 0, y2 = 10, y3 = 0, y4 = 10,
#'               col.plot = 10, row.plot = 20)
#' # Access the plot map
#' LTE.out$map
#'
#' # Access the plot
#' LTE.out$plot
#'
map.LTE <- function(x1, x2, x3, x4, y1, y2, y3, y4,
                    col.plot, row.plot, data = NULL, ...) {

  vertices <- matrix(c(x1, x2, x3, x4, y1, y2, y3, y4), ncol = 2, byrow = TRUE)
  field_polygon <- sp::Polygon(vertices)
  field_polygons <- sp::Polygons(list(field_polygon), 1)
  field_sp <- sp::SpatialPolygons(list(field_polygons))

  slope <- (y2 - y1) / (x2 - x1)
  angle <- atan(slope) * 180 / pi

  rotated_sp <- sp::elide(field_sp, rotate = angle, center = c(mean(c(x1, x2)), mean(c(y1, y2))))

  r <- raster::raster(nrows = row.plot, ncols = col.plot)
  r <- raster::setExtent(r, sp::bbox(rotated_sp))

  if (!is.null(data)) {
    r[] <- data
  } else {
    r[] <- 1:(row.plot * col.plot)
  }

  r_polygons <- raster::rasterToPolygons(r)
  rotated_r_polygons <- sp::elide(r_polygons, rotate = -angle, center = c(mean(c(x1, x2)), mean(c(y1, y2))))

  plot_map <- sp::SpatialPolygonsDataFrame(rotated_r_polygons, data.frame(ID = 1:length(rotated_r_polygons)))

  plot <- sp::plot(plot_map, main = "Field maps", col = rev(terrain.colors(64)), ...)

  # Capture the plot using recordPlot
  captured_plot <- recordPlot()

  list(map = plot_map, plot = captured_plot)
}


