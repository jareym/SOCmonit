#'  Apply Machine Learning regression algorithms
#'
#' Function for model building and validation using  Machine Learning regression algorithms.
#' @param spec The spectral data used for model fitting.
#' @param soc The target variable (soil organic carbon) used for model fitting.
#' @param fold.rep A list representing the data divided into folds for cross-validation. Obtained fro the CV.folds.list function.
#' @param nfold The number of folds for cross-validation (default is 5).
#' @param method The machine learning algorithm to be used (default is c("mars", "rf", "cubic")).
#' @param mars.param A list of parameters for MARS algorithm, including "degree" and "nprune".
#' @param rf.param Parameters for random forest algorithm (e.g., number of variables to consider at each split).
#' @param cubist.param Parameters for Cubist algorithm, including "committees" and "neighbors".
#' @param ext.rep The number of repetitions for the external cross-validation loop (default is 5).
#' @param int.rep The number of repetitions for the internal cross-validation loop (default is 5).
#' @param validation The validation method, currently only supports "CV" (cross-validation).
#' @param ... Additional arguments to be passed to the underlying model fitting functions.
#'
#' @return A list containing the predictions, fitted models, optimal parameters, and error metrics for each method.
#'
#' @examples
#' # Applying MARS with cross-validation
#' mars_params <- list(degree = 2, nprune = 5)
#' result <- apply.model.ml(spec, soc, fold.rep, method = "mars", mars.param = mars_params)
#'
#' # Applying random forest with cross-validation
#' rf_params <- 1:10
#' result <- apply.model.ml(spec, soc, fold.rep, method = "rf", rf.param = rf_params)
#'
#' # Applying Cubist with cross-validation
#' cubist_params <- list(committees = 5, neighbors = 10)
#' result <- apply.model.ml(spec, soc, fold.rep, method = "cubist", cubist.param = cubist_params)
#'
#' @export
#'


 apply.model.ml<- function (spec, soc,  fold.rep, nfold=5, method=c("mars", "rf", "cubic"), mars.param=list(degree, nprune),
                          rf.param, cubist.param=list(committees, neighbors), ext.rep=5, int.rep=5,
                          validation=c( "CV"), ...) {

   out.model<-list()
     if (method=="mars") {

      if (validation=="CV") {

        predict.model<-list()
        opt.param<-list()
        colnames(soc)[1:2]<- c("sample.ID","soc.value")
        soc.pred<-data.frame(soc[,1:2])


        ngrid <- expand.grid(mars.param$degree, mars.param$nprune)


        for(p in 1:ext.rep) {
          estimated<-NULL

          for(o in 1:nfold) {


            print(paste(p,o))

            name <-paste(p)
            name2 <-paste(o)
            spec.data<-cbind( fold.rep[[p]][,5:(5+nfold)], soc, spec)
            test<-subset(spec.data, fold==o)
            train<-subset(spec.data, fold!=o)




            d<-NULL

            for(m in 1:int.rep) {

              name3 <-paste(m)

              train.int<- subset(train, train[, 1+o] !=m)


              test.int<-subset(train, train[, 1+o] ==m)

              t.id<-test.int[,(nfold+2):(nfold+3)]


              for(h in 1:nrow(ngrid)) {
                m.mars.train <- earth(train.int$soc.value~., data=train.int[,-c(1:(nfold+3))], degree=ngrid[h,1],
                                      nprune=ngrid[h,2])
                predict.int<- predict(m.mars.train, newdata=test.int[,-c(1:(nfold+3))])

                t.id<- cbind(t.id, predict.int)

              }
              d<-rbind(d,t.id)


            }


            rmse.int<-NULL
            for(q in 1:nrow(ngrid)){
              e<-chillR::RMSEP( d[,q+2], d$soc.value)
              rmse.int<- rbind(rmse.int,e)

            }

            opt<-  ngrid   [which.min(rmse.int),]

            test.ext<- test[, -c(1:(nfold+3))]


            m.mars.predict <- earth(train$soc.value ~ ., data=train[,-c(1:(nfold+3))],
                                    degree=opt[,1], nprune=opt[,2])


            test.pred  <- predict(m.mars.predict, newdata = test.ext)

            predict.model[[name]][[name2]]<-m.mars.predict
            opt.param [[name]][[name2]]<-opt

            est<-data.frame(test$sample.ID, as.numeric(test.pred))
            estimated<- rbind (estimated, est)

          }

          soc.pred<-cbind(soc.pred, estimated[order(estimated$test.sample.ID),2])
          colnames(soc.pred)[2+p]<-paste0("pred.soc rep."," ", p)
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

      }
   }

   if (method=="rf") {

     if (validation=="CV") {

       predict.model<-list()
       opt.param<-list()
       colnames(soc)[1:2]<- c("sample.ID","soc.value")
       soc.pred<-data.frame(soc[,1:2])



       nmtry<-1:rf.param


       for(p in 1:ext.rep) {
         estimated<-NULL

         for(o in 1:nfold) {


           print(paste(p,o))

           name <-paste(p)
           name2 <-paste(o)
           spec.data<-cbind( fold.rep[[p]][,5:(5+nfold)], soc, spec)
           test<-subset(spec.data, fold==o)
           train<-subset(spec.data, fold!=o)



           d<-NULL


           for(m in 1:int.rep) {

             name3 <-paste(m)

             train.int<- subset(train, train[, 1+o] !=m)


             test.int<-subset(train, train[, 1+o] ==m)

             t.id<-test.int[,(nfold+2):(nfold+3)]

             h=1

             for(h in 1:rf.param) {
               m.rf.train <- randomForest(train.int$soc.value~., data=train.int[,-c(1:(nfold+3))], mtry=nmtry[h])
               predict.int<- predict(m.rf.train, newdata=test.int[,-c(1:(nfold+3))])#internal predictions

               t.id<- cbind(t.id, predict.int)

             }


             d<-rbind(d,t.id)


           }



           rmse.int<-NULL
           for(q in 1:rf.param){
             e<-chillR::RMSEP( d[,q+2], d$soc.value)
             rmse.int<- rbind(rmse.int,e)

           }

           opt<-  nmtry   [which.min(rmse.int)]

           test.ext<- test[, -c(1:(nfold+3))]


           m.rf.predict <- randomForest(train$soc.value ~ ., data=train[,-c(1:(nfold+3))],
                                   mtry=opt)


           test.pred  <- predict(m.rf.predict, newdata = test.ext)

           predict.model[[name]][[name2]]<-m.rf.predict
           opt.param [[name]][[name2]]<-opt

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

     }

   if (method=="cubist") {

     if (validation=="CV") {

       predict.model<-list()
       opt.param<-list()
       colnames(soc)[1:2]<- c("sample.ID","soc.value")
       soc.pred<-data.frame(soc[,1:2])


       ngrid <- expand.grid(cubist.param$committees, cubist.param$neighbors)


       for(p in 1:ext.rep) {
         estimated<-NULL

         for(o in 1:nfold) {


           print(paste(p,o))

           name <-paste(p)
           name2 <-paste(o)
           spec.data<-cbind( fold.rep[[p]][,5:(5+nfold)], soc, spec)
           test<-subset(spec.data, fold==o)
           train<-subset(spec.data, fold!=o)



           d<-NULL

           for(m in 1:int.rep) {

             name3 <-paste(m)

             train.int<- subset(train, train[, 1+o] !=m)


             test.int<-subset(train, train[, 1+o] ==m)

             t.id<-test.int[,(nfold+2):(nfold+3)]

             h=1

             for(h in 1:nrow(ngrid)) {
               m.cubist.train <- cubist(train.int[,-c(1:(nfold+3))],train.int$soc.value, committees=ngrid[h,1],
                                       neighbors=ngrid[h,2])
               predict.int<- predict(m.cubist.train, newdata=test.int[,-c(1:(nfold+3))])

               t.id<- cbind(t.id, predict.int)

             }
             d<-rbind(d,t.id)


           }



           rmse.int<-NULL
           for(q in 1:nrow(ngrid)){
             e<-chillR::RMSEP( d[,q+2], d$soc.value)
             rmse.int<- rbind(rmse.int,e)

           }

           opt<-  ngrid   [which.min(rmse.int),]

           test.ext<- test[, -c(1:(nfold+3))]


           m.cubist.predict <- cubist(train.int[,-c(1:(nfold+3))],train.int$soc.value,
                                     committees =opt[,1], neighbors=opt[,2])


           test.pred  <- predict(m.cubist.predict, newdata = test.ext)

           predict.model[[name]][[name2]]<-m.cubist.predict
           opt.param [[name]][[name2]]<-opt

           est<-data.frame(test$sample.ID, as.numeric(test.pred))
           estimated<- rbind (estimated, est)

         }

         soc.pred<-cbind(soc.pred, estimated[order(estimated$test.sample.ID),2])
         colnames(soc.pred)[2+p]<-paste0("pred.soc rep."," ", p)
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

       #R2
       r2<-NULL
       for(i in 1:ext.rep) {

         a<- summary(lm(soc.pred[,2+i]~soc.pred[,2]))$r.squared
         r2 <-rbind(r2, a)
         row.names(r2)[i]<-paste0("Rep.",i)

       }

     }

   }




    error.metric<-data.frame(rmse,r2, rpiq)
    names(error.metric)<-c("RMSE", "R2", "RPIQ")
    out.model[["Predictions"]] <- soc.pred
    out.model[["Models"]] <- predict.model
    out.model[["Opt. Param."]] <-  unlist(opt.param)
    out.model[["Error metrics"]] <- error.metric

    out.model

}
