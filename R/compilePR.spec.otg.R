#' Compile predictors from on-the-go data

#'
#' This function compiles predictors from on-the-go spatial data based on spatial points and a specified neighborhood size.
#' @param x On-the-go spatial data to compile predictors from.
#' @param sp.points Spatial points representing the neighbors.
#' @param nb Number of neighbors to include for each on-the-go data point.
#' @param ID.point Spatial points' identifier column.
#' @param ID.plot ID plot of the Land Type Entity (LTE) to match against.
#' @param LTE LTE spatial polygon to match against.
#' @param maxdist Maximum distance (optional) to consider when selecting neighbors. Only neighbors within this distance will be included.
#' @import sp
#' @export
#' @examples
#' # Compile predictors from on-the-go data
#' predictors <- compilePR.spec.otg(x = onthego_data, sp.points = spatial_points, nb = 5,
#'                                  ID.point = "ID_point", ID.plot = "ID_plot", LTE = lte_polygon)
#'

#' @return A data frame containing the compiled predictors. Each row represents an on-the-go data point, and the columns contain the selected neighbors' attributes and distances.
#'

compilePR.spec.otg <- function(x, sp.points, nb, ID.point, ID.plot, LTE, maxdist = NULL) {

  n <- match(ID.plot, names(LTE))
  names(LTE)[n] <- "ID.plot"
  x_coords <- sp::over(x, LTE)
  x$ID.plot <- x_coords[,"ID.plot"]
  x <- subset(x, !is.na(ID.plot))

  sp_points_coords <- sp::over(sp.points, LTE)
  sp.points$ID.plot <- sp_points_coords[,"ID.plot"]

  nb <- nb
  out.spec <- NULL
  for (i in 1:nrow(sp.points)) {
    plot.set <- subset(x, ID.plot %in% sp.points$ID.plot[i])
    p_set <-raster::union(sp.points[i, ], plot.set)
    p_set@data$distance <- as.matrix(dist(p_set@coords))[,
                                                         1]
    p_set@data$ID.point <- sp.points$ID.point[i]
    p_set@data$xcoord <- p_set@coords[, 1]
    p_set@data$ycoord <- p_set@coords[, 2]
    if (!is.null(maxdist)) {
      if (length(p_set) > nb) {
        selected_points <- p_set[order(as.matrix(dist(p_set@coords))[,
                                                                     1])[2:(nb + 1)], ]@data
        selected_points <- subset(selected_points, distance <=
                                    maxdist)
      }
      else {
        selected_points <- p_set[order(as.matrix(dist(p_set@coords))[,
                                                                     1])[2:(length(p_set))], ]@data
        selected_points <- subset(selected_points, distance <=
                                    maxdist)
      }
    }
    else {
      if (length(p_set) > nb) {
        selected_points <- p_set[order(as.matrix(dist(p_set@coords))[,
                                                                     1])[2:(nb + 1)], ]@data
      }
      else {
        selected_points <- p_set[order(as.matrix(dist(p_set@coords))[,
                                                                     1])[2:(length(p_set))], ]@data
      }
    }
    out.spec <- rbind(out.spec, selected_points)

  }
  out.spec

}
