#' Read LTE shapefile
#'
#' Read a Long-Term Experiment (LTE) shapefile and return the data.
#'
#' @param file Location of the shapefile.
#' @param ID.plot Column name or index with the plot ID.
#' @export
#' @import sp
#' @examples
#' out.LTE<-  rd.LTE(system.file("extdata/LTE.shp", package="SOCmonit"), ID.plot="ID_plot")

rd.LTE<- function (file, ID.plot=NULL, ...) {

  if (is.null(ID.plot))
  {
  out.LTE<-  raster::shapefile(file)
  out.LTE$ID.plot<- 1:nrow(out.LTE)
  p<-spplot(out.LTE, "ID.plot", main="ID.plot")


    }
  else{
  out.LTE<-  raster::shapefile(file)
   n<-which( colnames(out.LTE@data)== ID.plot )
   colnames(out.LTE@data) [n]<- "ID.plot"
      p<-spplot(out.LTE, "ID.plot", main="ID.plot")

   }
      print(p)
  out<-out.LTE

 }
