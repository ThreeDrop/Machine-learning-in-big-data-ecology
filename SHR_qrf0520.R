#library(gbm)
#library(dismo)
library(caret)
library(randomForest)
library(R.matlab)
library(quantregForest)
library(raster)
gppvars=c("GPP_mean","BESS_GPP","PmodelGPP","MODISGPP")
soilwvars=c("soilw","TWS","GLDAS","ESACCI")
can_vars=c('LC','MAT','MAP','MAR','Ndep','SC','TC','totalN','totalC')

file_gpp_name=c("/home/orchidee04/yyao/SHRtraining/annualGPP from jung 2017/mean annual GPP ",
                "/home/orchidee04/yyao/SHRtraining/BESSgpp/BESS annual GPP ",
                "/home/orchidee04/yyao/SHRtraining/pmodelGPP/pmodelGPP annual ",
                "/home/orchidee04/yyao/SHRtraining/MODISgpp/MODIS annual GPP ")
file_soilw_name=c("/home/orchidee04/yyao/SHRtraining/globalSoilw/global annual soilw ",
                  "/home/orchidee04/yyao/SHRtraining/GRACE reconstruction ann TWS/GRACE annual TWS ",
                  "/home/orchidee04/yyao/SHRtraining/annual_GLDAS/GLDAS annual weighted soilm ","/home/orchidee04/yyao/SHRtraining/ESACCI/ESACCI soilw ")

id_gpp_name=c("FLUXCOM","BESS","Pmodel","MODIS")
id_soilw_name=c("CPC","GRACE TWS","GLDAS","ESACCI")

gpp_begin=c(1985,2000,1985,2000)
gpp_end=c(2013,2013,2013,2015)
soilw_begin=c(1985,1985,1985,1985)
soilw_end=c(2013,2013,2013,2013)

sc05=readMat(paste("/home/orchidee04/yyao/SHRtraining/SC05.mat",sep=""))
annual_sc05=sc05$SC.05
tc05=readMat(paste("/home/orchidee04/yyao/SHRtraining/TC05.mat",sep=""))
annual_tc05=tc05$TC.05
totalN05=readMat(paste("/home/orchidee04/yyao/SHRtraining/totalN05.mat",sep=""))
annual_tN05=totalN05$totalN05

classLC=readMat(paste("/home/orchidee04/yyao/SHRtraining/onehot/onehot MODIS land cover yr ",2001,".mat",sep =""))
global_LC=classLC$onehot.LC

const_vars=c("tmp","pre","rad","Ndep","GPP","soilw")
#### isolate 
for (soilnum in 3:3) {
  for (gppnum in 1:1) {
    explain_vars=c(can_vars,gppvars[gppnum],soilwvars[soilnum])
    load(file = paste("/home/orchidee04/yyao/SHRtraining/qrf/QRF ",id_gpp_name[gppnum]," ",id_soilw_name[soilnum],".Rdata",sep=""))
    
    for (var in 6:6) {
        # max(gpp_begin[gppnum],soilw_begin[soilnum]):min(gpp_end[gppnum],soilw_end[soilnum]
		var_length=(min(gpp_end[gppnum],soilw_end[soilnum])-max(gpp_begin[gppnum]-soilw_begin[soilnum])+1)
		ave_all_var=rep(0,360*720*var_length)
		dim(ave_all_var)=c(360,720,var_length)
		var_begin=max(gpp_begin[gppnum],soilw_begin[soilnum])
		var_end=min(gpp_end[gppnum],soilw_end[soilnum])
		var_begin=1985
		var_end=2013
		for (var_year in 1985:2013) {
			   if (var==1) {
			tmp=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual tmp ",var_year,".mat",sep =""))
					annual_tmp=tmp$annual.tmp
			ave_all_var[,,var_year-var_begin+1]=annual_tmp	

				}
		   if (var==2) {
			pre=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual pre ",var_year,".mat",sep =""))
				annual_pre=pre$annual.pre
			ave_all_var[,,var_year-var_begin+1]=annual_pre

		}
		  if (var==3) {
			rad=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual rad ",var_year,".mat",sep =""))
				annual_rad=rad$annual.rad
			ave_all_var[,,var_year-var_begin+1]=annual_rad

			}
		 if (var==4) {
			ndep=readMat(paste("/home/orchidee04/yyao/SHRtraining/globalNdep/global annual Ndep ",var_year,".mat",sep =""))
				annual_Ndep=ndep$global.ann.Ndep
			ave_all_var[,,var_year-var_begin+1]=annual_Ndep
		}
		if (var==5) {
			GPP=readMat(paste(file_gpp_name[gppnum],var_year,".mat",sep=""))
				annual_GPP=GPP$annual.GPP.mean
			ave_all_var[,,var_year-var_begin+1]=annual_GPP
		}
		if (var==6) {
			soilw=readMat(paste(file_soilw_name[soilnum],var_year,".mat",sep =""))
				annual_soilw=soilw$global.ann.soilw
			ave_all_var[,,var_year-var_begin+1]=annual_soilw
			}
		}
		ave_var=apply(ave_all_var,c(1,2),mean)
        for (year in var_begin:var_end) {
        #ndvi_Maps=rep(0,360*720*11)
        #dim(ndvi_Maps)=c(360,720,11)
	
	
			year_rad=year
			year_tmp=year
			year_pre=year
			year_Ndep=year
			year_soilw=year
			year_GPP=year
#			initial_year=max(gpp_begin[gppnum],soilw_begin[soilnum])
#			eval(parse(text=paste("year_",const_vars[var],"=",initial_year,sep="")))
			rad=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual rad ",year_rad,".mat",sep =""))
			annual_rad=rad$annual.rad
			tmp=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual tmp ",year_tmp,".mat",sep =""))
			annual_tmp=tmp$annual.tmp
			pre=readMat(paste("/home/orchidee04/yyao/SHRtraining/CRUNCEPv8/cruncepv8 annual pre ",year_pre,".mat",sep =""))
			annual_pre=pre$annual.pre
			
			ndep=readMat(paste("/home/orchidee04/yyao/SHRtraining/globalNdep/global annual Ndep ",year_Ndep,".mat",sep =""))
			annual_Ndep=ndep$global.ann.Ndep
			
			soilw=readMat(paste(file_soilw_name[soilnum],year_soilw,".mat",sep =""))
			annual_soilw=soilw$global.ann.soilw
			GPP=readMat(paste(file_gpp_name[gppnum],year_GPP,".mat",sep=""))
			annual_GPP=GPP$annual.GPP.mean
			
			ndvi_Maps=data.frame(V1=as.data.frame(raster(global_LC)),V2=as.data.frame(raster(annual_tmp)))
			ndvi_Maps[,3]=as.data.frame(raster(annual_pre))
			ndvi_Maps[,4]=as.data.frame(raster(annual_rad))
			ndvi_Maps[,5]=as.data.frame(raster(annual_Ndep))
			ndvi_Maps[,6]=as.data.frame(raster(annual_sc05))
			ndvi_Maps[,7]=as.data.frame(raster(annual_tc05))
			ndvi_Maps[,8]=as.data.frame(raster(annual_tN05))
			ndvi_Maps[,9]=as.data.frame(raster(annual_sc05+annual_tc05))
			ndvi_Maps[,10]=as.data.frame(raster(annual_GPP))
			ndvi_Maps[,11]=as.data.frame(raster(annual_soilw))
			

			if (var<=4) {
				ndvi_Maps[,var+1]=as.data.frame(raster(ave_var))
			} else {
				ndvi_Maps[,var+5]=as.data.frame(raster(ave_var))
			}
			names(ndvi_Maps)<-c("LC","MAT","MAP","MAR","Ndep","SC","TC","totalN","totalC",gppvars[gppnum],soilwvars[soilnum])
			
			not.na.row=!is.na(ndvi_Maps[,1])
			for (cols in 2:11){
			  not.na.row=not.na.row & !is.na(ndvi_Maps[,cols])
			}
			new_maps=ndvi_Maps[not.na.row,]
			
			#ndvi_Maps[is.na(ndvi_Maps)]=0
			
			#p <- predict(qrf,new_maps,what=c(0.1,0.25,0.5,0.75,0.9))
			
			p_mean=predict(qrf,new_maps,what=mean)
			p_median=predict(qrf,new_maps,what=median)
			p_sd=predict(qrf,new_maps,what=sd)
			p_25=predict(qrf,new_maps,what=c(0.25))
			p_75=predict(qrf,new_maps,what=c(0.75))
			p_mean_std=rep(0,259200*3)
			dim(p_mean_std)=c(259200,3)
			p_quantiles=rep(0,259200*2)
			dim(p_quantiles)=c(259200,2)
			p_mean_std[not.na.row,1]=p_mean
			p_mean_std[not.na.row,2]=p_sd
			p_mean_std[not.na.row,3]=p_median
			p_quantiles[not.na.row,1]=p_25
			p_quantiles[not.na.row,2]=p_75
			rh_mean=(t(matrix(p_mean_std[,1],ncol=360)))
			rh_std=(t(matrix(p_mean_std[,2],ncol=360)))
			rh_median=(t(matrix(p_mean_std[,3],ncol=360)))
			rh_q25=(t(matrix(p_quantiles[,1],ncol=360)))
			rh_q75=(t(matrix(p_quantiles[,2],ncol=360)))
			writeMat(paste("/home/orchidee04/yyao/SHRtraining/SHR_estimate/QRF0618/qrfmedian const ",const_vars[var],' ',id_gpp_name[gppnum]," ",id_soilw_name[soilnum]," mean and sd annual RH year",year,".mat",sep=""),rh_mean=rh_mean,rh_std=rh_std,rh_median=rh_median,rh_q75=rh_q75,rh_q25=rh_q25)
		}
    }
  }
}  
