#' Generate a variogram model
#'
#' This function creates an experimental variogram and fits a variogram model to the experimental data.
#' It's used in geostatistical studies to quantify the spatial dependence of soil organic carbon (SOC).
#'
#' @param soc.values A numeric vector of soil organic carbon values.
#' @param xcoord A numeric vector of x coordinates.
#' @param ycoord A numeric vector of y coordinates.
#' @param v.model Variogram model type (as accepted by gstat::vgm).
#' @param parameters Initial estimates of the variogram parameters. Includes nugget, partial sill, and range.
#' @param ... Additional arguments to be passed to gstat::vgm and gstat::variogram.
#' @return A list containing the experimental variogram and the fitted variogram model.
#' @import gstat
#' @import sp
#' @import ggplot2
#' @export
#' @examples
#' # Assuming my_data is a dataframe with columns 'value', 'xcoord', 'ycoord'
#' # Apply the variogram model function
#' vgm_model <- apply.model.vgm(my_data$value, my_data$xcoord, my_data$ycoord, v.model = "Sph", parameters = c(0, 70000, 800))


apply.model.vgm <- function(soc.values, xcoord, ycoord, v.model,
                            parameters = c(nugget = 0, p.sill, range), ...) {


  data <- data.frame(xcoord, ycoord, soc.values)
  colnames(data) <- c("xcoord", "ycoord", "soc.values")
  sp::coordinates(data) <- ~xcoord + ycoord

  v.C. <- gstat::variogram(soc.values ~ 1, data = data, ...) # create experimental variogram
  m.C. <- gstat::vgm(parameters[2], v.model, parameters[3], parameters[1]) # initial guess variogram model
  m.C.f. <- gstat::fit.variogram(v.C., m.C., ...) # fit

  m.C.l. <- gstat::variogramLine(m.C., maxdist = max(v.C.$dist))
  m.C.f.l. <- gstat::variogramLine(m.C.f., maxdist = max(v.C.$dist))

  p <- ggplot(v.C., aes(x = dist, y = gamma)) +
    geom_point() +
    geom_line(data = m.C.f.l., aes(col = "Fitted")) +
    geom_line(data = m.C.l., aes(col = "Initial guess")) +
    xlab("Distance") + ylab("semivariance") +
    scale_colour_manual("", values = c("red", "black")) +
    theme(legend.position = "top", legend.key = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"))

  out.vgm <- list()
  out.vgm[["variogram"]] <- v.C.
  out.vgm[["model"]] <- m.C.f.

  print(p)
  out.vgm

}



#'  Apply kriging interpolation methods
#'
#' Function to apply kriging spatial interpolation
#' @param x spatial object with variable values
#' @param soc.values variable to be interpolated
#' @param method interpolation method: OK(ordinary kriging ) or BK (block kriging)
#' @param newdata new spatial data to be interpolated
#' @param v.model variogram model
#' @param block adding a block for block kriging
#' @param ... additional arguments of krige
#' @import gstat
#' @import sp
#' @import gridExtra
#' @export
#' @examples
#' # Assuming my_grid_data is a SpatialPixelsDataFrame for prediction
#' # Apply kriging using the variogram model
#' krige_result <- apply.model.krige(my_data, "value", method = "OK", newdata = my_grid_data, v.model = vgm_model$model)
#'

apply.model.krige <- function (x, soc.values, method=c("OK", "BK"), newdata, v.model, block=NULL, ...) {

  n <- match(soc.values, names(x))
  names(x)[n] <- "soc.values"

  if (method == "OK") {
    m.krige <- gstat::krige(soc.values~1, locations=x, newdata=newdata, model=v.model, ...)
  }

  if (method == "BK") {
    m.krige <- gstat::krige(soc.values~1, locations=x, newdata=newdata, model=v.model, block=block, ...)
  }

  map <- m.krige

  if (isTRUE(class(map)=="SpatialPointsDataFrame")) {
    map <- SpatialPixelsDataFrame(m.krige@coords, m.krige@data) # convert spatial grid  to pixel data
  }

  color1 = colorRampPalette(c("lightyellow","yellow","orange","brown")) # color scale
  color2 = colorRampPalette(c("blue", "cyan", "yellow","orange","red")) # color scale

  p1 <- spplot(map, "var1.pred", col.regions=color1(16), scales=list(draw = TRUE), main="Prediction")
  p2 <- spplot(map, "var1.var", col.regions=color2(16), scales=list(draw = TRUE), main="Variance")

  gridExtra::grid.arrange(nrow=1, p1, p2)

  out.krige <- m.krige
  out.krige
}








