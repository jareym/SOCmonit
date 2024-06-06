#' CV fold division list
#'
#' This function creates a list of training and test sets for cross-validation using the spatially aware k-fold
#' sampling method provided by the CV.folds function. The cross-validation process is conducted with multiple
#' repetitions at both external and internal levels.
#'
#' External repetitions divide the entire dataset into k-folds. For each external repetition, internal repetitions
#' are performed, where each internal repetition divides the remaining data (excluding one of the external folds)
#' into k-folds again. This results in a nested cross-validation structure that helps provide a robust estimation
#' of model performance and stability across different partitionings of the data.
#'
#' @param nfold Number of folds.
#' @param ext.rep Number of external repetitions.
#' @param int.rep Number of internal repetitions.
#' @param min.dist Minimum distance threshold for neighbor selection in the CV.folds function.
#' @param ID.point Column of unique identifiers for the data points.
#' @param soc.values Column of the variable values.
#' @param xcoord Column of x-axis coordinates of the data points.
#' @param ycoord Column of y-axis coordinates of the data points.
#' @param stratified Logical indicating whether to perform stratified sampling based on quantiles.
#'   Stratified sampling ensures that each fold is a good representative of the whole dataset.
#' @export
#' @examples
#' # Generate CV fold division list
#' cv_list <- CV.folds.list(nfold = 5, ext.rep = 3, int.rep = 2, min.dist = 0.5,
#'                          ID.point = df$ID, soc.values = df$SOC,
#'                          xcoord = df$x, ycoord = df$y, stratified = T)
#'
#' @details Each iteration of the internal repetitions produces a new fold assignment (named as "fold.int [iteration]")
#' added to the data. The resulting list contains entries for each external repetition. Each entry in the list is a
#' data frame with the original data, fold assignments from the external repetition (named "fold"), and fold
#' assignments from all internal repetitions.

CV.folds.list<- function (nfold, ext.rep, int.rep, min.dist, ID.point,
                        soc.values, xcoord, ycoord, stratified = T) {


  fold.rep.all<-list()#external and internal division



  for(i in 1:ext.rep){#external repetitions

    sink("NUL")
    rep<-CV.folds(soc.values,  xcoord=xcoord, ycoord=ycoord, nfolds=nfold, min.dist = min.dist, stratified)
    sink()
    soc.int<-data.frame(rep,xcoord, ycoord)

    for(j in 1:int.rep)# internal division

    {
      soc.int.a<- subset(soc.int, fold!= j)
      soc.int.b <- subset(soc.int, fold== j)
      sink("NUL")

       rep.int<-CV.folds(soc.int.a[,1], xcoord=soc.int.a[,4], ycoord=soc.int.a[,5],
                        nfolds=nfold, min.dist =min.dist, stratified)
       sink()

      soc.int.a[,5+j]<-rep.int$fold
      soc.int.b[,5+j]<-0
      soc.int.all<-rbind(soc.int.a, soc.int.b)
      soc.int<-soc.int.all[order(soc.int.all[,2]),]
      colnames(soc.int)[5+j]<-paste0("fold.int"," ", j)

    }


    fold.rep.all[[i]]<-cbind(ID.point, soc.values, xcoord, ycoord, soc.int[,c(3, 6:(int.rep+5))])

  }

  fold.rep.all
}

