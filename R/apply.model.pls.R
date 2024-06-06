#' Apply Partial Least Squares regression
#'
#' Function for model building and validation using Partial Least Squares regression
#'
#' @param spec The spectral data.
#' @param soc The sample ID and soil organic carbon data.
#' @param ext.rep The number of external repetitions.
#' @param int.rep The number of internal repetitions.
#' @param validation The method of validation (currently supports cross-validation only).
#' @param ncomp The number of components.
#' @param fold.rep A list representing the data divided into folds for cross-validation. Obtained fro the CV.folds.list function.
#' @param nfold The number of folds (should be the same as the number of folds in the `fold.rep` object).
#' @param ... additional arguments passed to \link[pls]{plsr}
#'
#' @import pls
#' @export
#'
#' @examples
#' # Load data
#' data <- SOCmonit:::data.spec
#' folds <- SOCmonit:::fold.rep
#' spec.data <- data[, -c(1:2)]
#' soc.data <- data[, c(1:2)]
#'
#' # Run the model
#' model.pls <- apply.model.pls(spec = spec.data, soc = soc.data, fold.rep = folds, ncomp = 25, ext.rep = 5, int.rep = 5, nfold = 5)
#' model.pls$Predictions # predictions output
#' model.pls$Models # individual model for each fold
#' model.pls$`Opt. Comp.` # optimal number of components for each model
#' model.pls$`Error metrics` # error metrics output



  apply.model.pls<- function (spec, soc,  fold.rep, ncomp, ext.rep=5, int.rep=5, nfold=5,
                          validation=c("CV"), ...) {

   out.model<-list()


      if (validation=="CV") {

    predict.model<-list()
    opt.compt<-list()
    colnames(soc)[1:2]<- c("sample.ID","soc.value")
    soc.pred<-data.frame(soc[,1:2])



    for(p in 1:ext.rep) {
      estimated<-NULL

      for(o in 1:nfold) {


        print(paste(p,o))

        name <-paste(p)
        name2 <-paste(o)
        spec.data<-cbind( fold.rep[[p]][,5:(5+nfold)], soc, spec)
        test<-subset(spec.data, fold==o)
        train<-subset(spec.data, fold!=o)
        train.x<-train[, -c(1:(nfold+3))]
        x<- (as.matrix(train.x))
        y<-(train$soc.value)

        d<-NULL

        for(m in 1:int.rep) {

          name3 <-paste(m)
          y.int<-as.numeric(unlist(subset(train, train[, 1+o] != m, select=soc.value)))
          x.int<-subset(train, train[, 1+o] !=m)
          x.int<-as.matrix(x.int[, -c(1:(nfold+3))])
          m.pls.train <- plsr(y.int ~ x.int, ncomp=ncomp)

          y.i<- as.numeric(unlist(subset(train, train[, 1+o] == m, select=soc.value)))
          x.i<- subset(train, train[, 1+o] == m)
          t.id<-x.i[, 7:8]
          x.i<-as.matrix(x.i[, -c(1:(nfold+3))])

          predict.int<- predict(m.pls.train, ncomp=1:ncomp, newdata= x.i)

          d<-rbind(d,data.frame(predict.int,t.id))

        }

        rmse.int<-NULL
        for(q in 1:ncomp){
          e<-chillR::RMSEP( d[,q], d$soc.value)
          rmse.int<- rbind(rmse.int,e)

        }
        opt.comp<-     which.min(rmse.int)

        x.test<- (as.matrix(test[, -c(1:(nfold+3))]))


        m.pls.predict <- pls::plsr(y ~ x, ncomp=opt.comp, ...)
        test.pred  <- predict(m.pls.predict, newdata = x.test, ncomp = opt.comp, ...)

        predict.model[[name]][[name2]]<-m.pls.predict
        opt.compt [[name]][[name2]]<-opt.comp

        est<-data.frame(test$sample.ID, as.numeric(test.pred))
        estimated<- rbind (estimated, est)

      }

      soc.pred<-cbind(soc.pred, estimated[order(estimated$test.sample.ID),2])
      colnames(soc.pred)[2+p]<-paste0("pred.soc rep."," ", p)
       }

     }

    rmse<-NULL
    for(i in 1:ext.rep) {

      a<- chillR::RMSEP(soc.pred[,2+i],soc.pred[,2])
      rmse <-rbind(rmse, a)
      row.names(rmse)<-NULL
    }


    rpiq<-NULL
    for(i in 1:ext.rep) {

      a<- chillR::RPIQ(soc.pred[,2+i],soc.pred[,2])
      rpiq <-rbind(rpiq, a)
      row.names(rpiq)<-NULL
    }

    r2<-NULL
    for(i in 1:ext.rep) {

      a<- summary(lm(soc.pred[,2+i]~soc.pred[,2]))$r.squared
      r2 <-rbind(r2, a)
      row.names(r2)[i]<-paste0("Rep.",i)
    }


    error.metric<-data.frame(rmse,r2, rpiq)
    names(error.metric)<-c("RMSE", "R2", "RPIQ")

    out.model[["Predictions"]] <- soc.pred
    out.model[["Models"]] <- predict.model
    out.model[["Opt. Comp."]] <-  unlist(opt.compt)
    out.model[["Error metrics"]] <- error.metric

    out.model

     }
