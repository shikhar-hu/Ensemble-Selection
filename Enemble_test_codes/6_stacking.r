
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
# i=1
	setwd(paste(home,ds[i,"name"],"/",sep=''))

	preproc_dir=paste(home,ds[i,"name"],"/","preproc_data",sep='')
	dir.create("candidates_val")
	candidates_val=paste(home,ds[i,"name"],"/","candidates_val",sep='')
	dir.create("candidates_test")
	candidates_test=paste(home,ds[i,"name"],"/","candidates_test",sep='')
	
	dir.create("ensemble_results")

	ensemble_results=paste(home,ds[i,"name"],"/","ensemble_results",sep='')
	setwd(ensemble_results)

	candidates_val=paste(home,ds[i,"name"],"/","candidates_val",sep='')

	candidates_test=paste(home,ds[i,"name"],"/","candidates_test",sep='')


	library(ROCR)

	setwd(candidates_val)


	ensembleSource_val = "all_val.csv"


	library(verification)


	## AUC
	auc <- function (obs, pred)
	{
	  out <- (roc.area(as.numeric(obs), as.numeric(pred))$A)

	  out
	}



	### Brier
	auc <- function (obs, pred)
	{

		out<- (brier(as.numeric(obs), as.numeric(pred))$bs)

	  out
	}



	setwd(candidates_val)

	ensembleSource=read.csv(ensembleSource_val,as.is=T)
	setwd(candidates_test)
	ensembleSource_test = "all_test.csv"


	ensembleTest=read.csv(ensembleSource_test,as.is=T)

	library(stepPlr)

	lr_model <- plr(x = ensembleSource[,-c(1:2)],y = as.numeric(ensembleSource[,2]),lambda=2^-15, cp="aic")


	lr_train <- predict(lr_model,ensembleSource[,-c(1:2)],type="response")
	lr_val <- predict(lr_model,ensembleTest[,-c(1:2)],type="response")



	auc(ensembleSource[,2],lr_train)


	auc(ensembleTest[,2],lr_val)




	library(randomForest)

	rf_model <- randomForest(x = ensembleSource[,-c(1:2)],y =  as.factor(ensembleSource[,2]), ntree=1200,mtry=floor((ncol(ensembleSource)-2)/6),nodesize=5)


	rf_train <- predict(rf_model, ensembleSource[,-c(1:2)], type="prob")[,2]


	colnames(ensembleTest)<-gsub("test","val",colnames(ensembleTest))
	rf_val <- predict(rf_model, ensembleTest[,-c(1:2)], type="prob")[,2]



	auc(ensembleSource[,2],rf_train)

	auc(ensembleTest[,2],rf_val)

}

