
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

	library(randomForest)
	
	setwd(preproc_dir)
	myFiles2 <- list.files(pattern="dfold.*csv")
	myFiles <- myFiles2 #[grep("g1|g2|g3|g4",myFiles2)]

	

	##### SVMRADIAL_model 
	library(e1071) #http://www.inside-r.org/node/57517

	setwd(preproc_dir)
	
	SVMRadial_parameters=expand.grid(degree = c(1,2,3,4), cost = 2^seq(-10,10,2))
		# lambda	
		# regularization parameter for the L2 norm of the coefficients. The minimizing criterion in plr is -log-likelihood+λ*\|β\|^2. Default is lambda=1e-4.
		# cp	
		# complexity parameter to be used when computing the score. score=deviance+cp*df. If cp="aic" or cp="bic", these are converted to cp=2 or cp=log(sample size).
	
pb <- winProgressBar(title = paste("SVMR progress bar for ",ds[i,"name"]), min = 0, max = length(myFiles), width = 400)


# for (j in 1:5)
	for (j in 1:length(myFiles))
	{
	# for (k in 1:5)
		for (k in 1:nrow(SVMRadial_parameters))
		{
			a=read.csv(paste(preproc_dir,'/',myFiles[j],sep=''),as.is=T)
			colnames(a)<-paste(gsub(".","",colnames(a),fixed=TRUE))
			val_data= read.csv(paste(preproc_dir,'/',"val.csv",sep=''),as.is=T)
			colnames(val_data)<-paste(gsub(".","",colnames(val_data),fixed=TRUE))
			test_data= read.csv(paste(preproc_dir,'/',"test.csv",sep=''),as.is=T)
			colnames(test_data)<-paste(gsub(".","",colnames(test_data),fixed=TRUE))
		
				x = a[,-c(1:2)]
			
			set.seed(3456)
			SVMRadial_model <- svm(x = a[,-c(1:2)], y = (a[,2]), scale = TRUE, type = NULL, kernel ="radial", degree = SVMRadial_parameters[k,1], gamma = if (is.vector(x)) 1 else 1 / ncol(x),coef0 = 0, cost = SVMRadial_parameters[k,2], nu = 0.5,class.weights= NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,shrinking = FALSE, cross = 0, probability = FALSE, fitted = FALSE)


			val_data$predictions = predict(SVMRadial_model, val_data[,-c(1:2)])

			## @@ platt's scaling
			calib.data.frame <- data.frame(cbind( val_data$predictions,val_data$dv))
			colnames(calib.data.frame) <- c("x", "y")
			
			set.seed(3456)
			calib.model <- glm(y ~ x, calib.data.frame, family=binomial)

			colnames(calib.data.frame) <- c("x")

			val_data$predictions <-predict(calib.model, newdata=calib.data.frame, type="response")

			setwd(candidates_val)
			df=val_data[ , c("id","predictions")]
		
			
			test_data= read.csv(paste(preproc_dir,'/',"test.csv",sep=''),as.is=T)
			colnames(test_data)<-paste(gsub(".","",colnames(test_data),fixed=TRUE))
			test_data$predictions =predict(SVMRadial_model, test_data[,-c(1:2)])

			## @@ platt's scaling
			calib.data.frame <- data.frame(cbind( test_data$predictions,test_data$dv))
			colnames(calib.data.frame) <- c("x", "y")
			
			set.seed(3456)
			calib.model <- glm(y ~ x, calib.data.frame, family=binomial)

			colnames(calib.data.frame) <- c("x")

			test_data$predictions <-predict(calib.model, newdata=calib.data.frame, type="response")

			setwd(candidates_test)
			df2=test_data[ , c("id","predictions")]
			
			if(k==1)
			{
				val_local=df
				colnames(val_local)[2]<-paste("svmr_val",j,k,sep="_")
				test_local=df2
				colnames(test_local)[2]<-paste("svmr_test",j,k,sep="_")	
			}else
			{
				val_local=cbind(val_local,z=df$predictions)
				colnames(val_local)[ncol(val_local)]<-paste("svmr_val",j,k,sep="_")
				test_local=cbind(test_local,df2$predictions)
				colnames(test_local)[ncol(test_local)]<-paste("svmr_test",j,k,sep="_")
			}
		

		}
	
	if(j==1)
		{
			val_global=val_local
			test_global=test_local

		}else
		{
			val_global=cbind(val_global,val_local)

			test_global=cbind(test_global,test_local)
		
		}
		
		setWinProgressBar(pb,j, title=paste(ds[i,"name"]," SVMR :", round(j/length(myFiles)*100, 0),"% done"))
	
	}

setwd(candidates_val)
write.csv (val_global[, -grep("id", colnames(val_global))],"svmradial_val_all.csv",row.names=F)


setwd(candidates_test)
write.csv (test_global[, -grep("id", colnames(test_global))],"svmradial_test_all.csv",row.names=F)
close(pb)

}

