
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
 
	

	##### Naive Bayes

	
	nb_parameters=expand.grid(fL =c(0,1,2,5,10) , usekernel = c("F","T"))
# nb_parameters=nb_parameters[1:5,]
pb <- winProgressBar(title = paste("NB progress bar for ",ds[i,"name"]), min = 0, max = length(myFiles), width = 400)

# for (j in 1:2)
	for (j in 1:length(myFiles))
	{
		a=read.csv(paste(preproc_dir,'/',myFiles[j],sep=''),as.is=T)
		colnames(a)<-paste(gsub(".","",colnames(a),fixed=TRUE))
		a$dv<-gsub('0','r',a$dv)
		a$dv<-gsub('1','s',a$dv)
		val_data= read.csv(paste(preproc_dir,'/',"val.csv",sep=''),as.is=T)
		colnames(val_data)<-paste(gsub(".","",colnames(val_data),fixed=TRUE))
		test_data= read.csv(paste(preproc_dir,'/',"test.csv",sep=''),as.is=T)
		colnames(test_data)<-paste(gsub(".","",colnames(test_data),fixed=TRUE))

		

		cl<-makeCluster(3) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		nb_model <- foreach(k = 1:nrow(nb_parameters),.packages=c("caret","kknn")) %dopar% {
			
						train(x = a[,-c(1:2)], y = as.factor(a[,2]),method='nb',tuneGrid = data.frame(fL=nb_parameters[k,1], usekernel=(nb_parameters[k,2]=="T")))
						}
		stopCluster(cl)

		cl<-makeCluster(2) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		nb_val <- foreach(k = 1:nrow(nb_parameters), .combine='cbind', .packages=c("caret","kknn")) %dopar% {
			predictions = predict(nb_model[[k]], val_data[,-c(1:2)], type="prob")[,2]
					
					}                
		stopCluster(cl)
		
		val_out_local=as.data.frame(nb_val)
		colnames(val_out_local) <- paste(gsub(".","_",colnames(nb_val),fixed=TRUE))
		colnames(val_out_local) <- paste(gsub("result",paste("nb_val",j,sep="_"),colnames(nb_val),fixed=TRUE))
		if(j==1)
			{nb_val_full=val_out_local
		}else
		{nb_val_full=cbind(nb_val_full,val_out_local)}
	
		cl<-makeCluster(2) #change the 2 to your number of CPU cores  
		registerDoSNOW(cl)  

		nb_test <- foreach(k = 1:nrow(nb_parameters), .combine='cbind', .packages=c("caret","kknn")) %dopar% {
			predict(nb_model[[k]], test_data[,-c(1:2)], type="prob")[,2]
						}                
		stopCluster(cl)
	
		test_out_local=as.data.frame(nb_test)
		colnames(test_out_local) <- paste(gsub(".","_",colnames(nb_test),fixed=TRUE))
		colnames(test_out_local) <- paste(gsub("result",paste("nb_test",j,sep="_"),colnames(nb_test),fixed=TRUE))
		if(j==1)
			{nb_test_full=test_out_local
		}else
		{nb_test_full=cbind(nb_test_full,test_out_local)}
	
		setWinProgressBar(pb,j, title=paste(ds[i,"name"]," NB :", round(j/length(myFiles)*100, 0),"% done"))
	}
	
	setwd(candidates_val)
	write.csv(nb_val_full,"nb_val.csv",row.names=F)
	
	setwd(candidates_test)
	write.csv(nb_test_full,"nb_test.csv",row.names=F)
	
	setwd(home)
close(pb)

}
