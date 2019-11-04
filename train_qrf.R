library(caret)
library(randomForest)
library(R.matlab)
library(quantregForest)

load("/home/orchidee04/yyao/SHRtraining/newtestSRDB.Rdata")
GPPname=c("GPP_1","GPP_2","GPP_3","GPP_4","GPP_5","GPP_6","PmodelGPP")
soilWname=c("soilw","TWS","GLDAS","ESACCI")
agg.data$totalC=agg.data$SC+agg.data$TC
agg.data=agg.data[agg.data$RH<1550,]
#folds<-createFolds(y=agg.data$RH,k=10)

id_gpp_name=c("GPP_1","GPP_2","GPP_3","GPP_4","GPP_5","GPP_6","PmodelGPP")
id_soilw_name=c("soilw","TWS","GLDAS")


for (num_soilw in 1:3) {
for (num_gpp in 1:7) {
     var_series=c('LC','MAT','MAP','MAR','Ndep','SC','TC','totalN','totalC',GPPname[num_gpp],soilWname[num_soilw]) # subsoil
     Xtrain=agg.data[,var_series]
     Ytrain=agg.data$RH
     qrf <- quantregForest(x=Xtrain, y=Ytrain, ntree=3000,
                            keep.forest=TRUE, importance=TRUE)
     save(qrf, file =paste("/home/orchidee04/yyao/SHRtraining/newclimate/qrfmodel/QRF CRUold ",id_gpp_name[num_gpp]," ",id_soilw_name[num_soilw],".Rdata",sep=""))
     gc(qrf)
     rm(qrf)
}
}
