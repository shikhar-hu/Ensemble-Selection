
rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")
setwd(Desktop)

home=paste(Desktop,"MEMS/S6/NIC/Datasets/",sep="")
setwd(home)


ds=read.csv("data_specs.csv",as.is=T)
rows= c(1:4)

for(i in rows)
{

	setwd(paste(home,ds[i,"name"],"/",sep=''))

	preproc_dir=paste(home,ds[i,"name"],"/","preproc_data",sep='')
	dir.create("candidates_val")
	candidates_val=paste(home,ds[i,"name"],"/","candidates_val",sep='')
	dir.create("candidates_test")
	candidates_test=paste(home,ds[i,"name"],"/","candidates_test",sep='')
	
	
	### install.packages('ggplot2',repos='http://mirrors.softliste.de/cran/',dependencies=T)
	library(caret)
	library(gbm)
	library(nnet)
	library(pROC)
	library(hmeasure) 
	library(dplyr)

	library(doSNOW)  
	library(foreach)  


	library(randomForest)
	
	setwd(preproc_dir)
	myFiles2 <- list.files(pattern="dfold.*csv")
	myFiles <- myFiles2 #[grep("g1|g2|g3|g4",myFiles2)]

	


	##### GBM
	library(gbm)
	setwd(preproc_dir)
	
	gbm_parameters=expand.grid(ntree =seq(100,2200,300), shrinkage=c(0.005,0.01,0.02),minobsinnode=c(9,11),ntrain=c(0.7,0.9))
# gbm_parameters=gbm_parameters[1:5]
	


pb <- winProgressBar(title = paste("GBM progress bar for ",ds[i,"name"]), min = 0, max = length(myFiles), width = 400)
	
# for (j in 1:2)
	for (j in 1:length(myFiles))
	{

		a=read.csv(paste(preproc_dir,'/',myFiles[j],sep=''),as.is=T)
		colnames(a)<-paste(gsub(".","",colnames(a),fixed=TRUE))
		val_data= read.csv(paste(preproc_dir,'/',"val.csv",sep=''),as.is=T)
		colnames(val_data)<-paste(gsub(".","",colnames(val_data),fixed=TRUE))
		test_data= read.csv(paste(preproc_dir,'/',"test.csv",sep=''),as.is=T)
		colnames(test_data)<-paste(gsub(".","",colnames(test_data),fixed=TRUE))
		
			

		cl<-makeCluster(3) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		GBM_model <- foreach(k = 1:nrow(gbm_parameters),.packages="gbm") %dopar% {

			
			
			GBM_model = gbm.fit(  x = a[,-c(1:2)], y = (a[,2]) , distribution = "bernoulli"  , n.trees =gbm_parameters[k,1]  , shrinkage = gbm_parameters[k,2] , interaction.depth = 1, n.minobsinnode = gbm_parameters[k,3], nTrain = round(nrow(a) * gbm_parameters[k,4])) 

						}
		stopCluster(cl)

		cl<-makeCluster(3) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		gbm_val <- foreach(k = 1:nrow(gbm_parameters), .combine='cbind', .packages="gbm") %dopar% {
			predict(object = GBM_model[[k]],newdata =val_data[,-c(1:2),], n.trees = gbm.perf(GBM_model[[k]], plot.it = FALSE), type = "response")
					}                
		stopCluster(cl)
		
		val_out_local=as.data.frame(gbm_val)
		colnames(val_out_local) <- paste(gsub(".","_",colnames(gbm_val),fixed=TRUE))
		colnames(val_out_local) <- paste(gsub("result",paste("gbm_val",j,sep="_"),colnames(gbm_val),fixed=TRUE))
		if(j==1)
			{gbm_val_full=val_out_local
		}else
		{gbm_val_full=cbind(gbm_val_full,val_out_local)}
	
		cl<-makeCluster(3) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		gbm_test <- foreach(k = 1:nrow(gbm_parameters), .combine='cbind', .packages="gbm") %dopar% {
			predict(object = GBM_model[[k]],newdata =test_data[,-c(1:2),], n.trees = gbm.perf(GBM_model[[k]], plot.it = FALSE), type = "response")
						}                
		stopCluster(cl)
	
		test_out_local=as.data.frame(gbm_test)
		colnames(test_out_local) <- paste(gsub(".","_",colnames(gbm_test),fixed=TRUE))
		colnames(test_out_local) <- paste(gsub("result",paste("gbm_test",j,sep="_"),colnames(gbm_test),fixed=TRUE))
		if(j==1)
			{gbm_test_full=test_out_local
		}else
		{gbm_test_full=cbind(gbm_test_full,test_out_local)}
	
		setWinProgressBar(pb,j, title=paste(ds[i,"name"]," GBM :", round(j/length(myFiles)*100, 0),"% done"))
	}
	
	setwd(candidates_val)
	write.csv(gbm_val_full,"gbm_val.csv",row.names=F)
	
	setwd(candidates_test)
	write.csv(gbm_test_full,"gbm_test.csv",row.names=F)
	
	setwd(home)
	close(pb)
}