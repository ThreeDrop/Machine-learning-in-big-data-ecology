#! /usr/bin/env Rscript

library(caret)
library(randomForest)
library(R.matlab)
library(quantregForest)

load("/home/orchidee04/yyao/SHRtraining/newtestSRDB.Rdata")
GPPname=c("GPP_mean","BESS_GPP","PmodelGPP","MODISGPP")
soilWname=c("soilw","TWS","GLDAS","ESACCI")
agg.data$totalC=agg.data$SC+agg.data$TC
agg.data=agg.data[agg.data$RH<1550,]
#folds<-createFolds(y=agg.data$RH,k=10)
for (num_gpp in c(1,3)) {
    for (num_soilw in 3:3) {
        var_series=c('LC','MAT','MAP','MAR','Ndep','totalN','totalC',GPPname[num_gpp],soilWname[num_soilw]) # subsoil
        cv_all_obs=data.frame()
        cv_all_pred=data.frame()
        for (k in 1:455) {
          cv_train=agg.data[-k,] # constant folds
          cv_test=agg.data[k,]
          mymodel <- train(as.data.frame(cv_train[,var_series]), as.vector(as.matrix(cv_train[,"RH"])), 
                           method = "rf", num.trees=200)
          
          cv_train_preds=predict(mymodel,cv_train)
          cv_test_preds=predict(mymodel,cv_test)
          
          cv_test_obs=cv_test$RH
          
          cv_all_obs=rbind(cv_all_obs,as.data.frame(cv_test_obs))
          cv_all_pred=rbind(cv_all_pred,as.data.frame(cv_test_preds))
        } 
            
            #cor(cv_all_obs,cv_all_pred)^2
        #plot(cv_all_obs$cv_test_obs,cv_all_pred$cv_test_preds,ylim=c(0,1500),xlim=c(0,1500),xlab="Observed annual SHR (gC m-2yr-1)",ylab="Predicted annual SHR (gCm-2yr-1)",cex.lab=2,cex.axis=2)
        #abline(0,1,lwd=2,lty=2)
        writeMat(paste("/home/orchidee04/yyao/SHRtraining/LOOCV ",GPPname[num_gpp]," ",soilWname[num_soilw],".mat",sep=""),cv_all_obs=as.data.frame(cv_all_obs),cv_all_pred=as.data.frame(cv_all_pred))
    }
}
