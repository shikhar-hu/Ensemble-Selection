
rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")
setwd(Desktop)

dir.create(paste(Desktop,"/MEMS",sep=""))
dir.create(paste(Desktop,"/MEMS/S6/NIC",sep=""))
dir.create(paste(Desktop,"/MEMS/S6/NIC/Datasets",sep=""))

home=paste(Desktop,"MEMS/S6/NIC/Datasets/",sep="")
setwd(home)


ds=read.csv("data_specs.csv",as.is=T)
rows= c(1:4)

for(i in rows)
{

	setwd(paste(home,ds[i,"name"],"/",sep=''))

	training_data= read.csv("preproc_data/full_train.csv",as.is=T)


	### 10 folds creation

	dat_dv=training_data[,c("dv","id")]



	##Splitting into test and train

	##Creating Local Test data set for model validation purposes
	# library(caret)
	# set.seed(3456)
	#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
	# devIndex <- as.data.frame(createFolds(dat_dv$dv, k=10,  list = TRUE))
	###Throwing error

	###Creating folds manually with control on dv
	set.seed(243)
	rand_num=c(sample(c(1:length(which(dat_dv$dv==0)))),sample(c(1:length(which(dat_dv$dv==1)))))
	dat_dv2=cbind(dat_dv[order(dat_dv[,'dv']),],rand_num)
	dat_dv2_0=dat_dv2[dat_dv2$dv==0,'rand_num']
	dat_dv2_0=as.data.frame(dat_dv2_0)
	dat_dv2_1=dat_dv2[dat_dv2$dv==1,'rand_num']
	dat_dv2_1=as.data.frame(dat_dv2_1)


	breaks=unique(quantile(dat_dv2_0[,1], probs = seq(0, 1, by= 0.2)))
	dat_dv2_0[,paste(colnames(dat_dv2_0[1]),"bin",sep="_")] <- cut(dat_dv2_0[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
	colnames(dat_dv2_0)[1]<-paste("rand_num")
	colnames(dat_dv2_0)[2]<-paste("bin")


	breaks=unique(quantile(dat_dv2_1[,1], probs = seq(0, 1, by= 0.2)))
	dat_dv2_1[,paste(colnames(dat_dv2_1[1]),"bin",sep="_")] <- cut(dat_dv2_1[,1], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
	colnames(dat_dv2_1)[1]<-paste("rand_num")
	colnames(dat_dv2_1)[2]<-paste("bin")

	dat_dv2_bin=rbind(dat_dv2_0,dat_dv2_1)

	dat_dv2=cbind(dat_dv2,dat_dv2_bin)
	dat_dv2=dat_dv2[,c(1,2,5)]
	dat_dv2=dat_dv2[order(dat_dv2[,'id']),]

		for(j in 1:5)
		{
		assign(paste("data_fold_",j,sep=''), dat_dv2[ dat_dv2$bin!=j, c("id","dv")])
		}
	#########


	dflist <- list(data_fold_1,data_fold_2,data_fold_3,data_fold_4,data_fold_5) #,data_fold_6,data_fold_7,data_fold_8,data_fold_9,data_fold_10)
	max_row_num=as.integer(max(as.character(lapply(dflist,function(x)nrow(x)))))


	###Creating bootstrap samples


		for(k in 1:5) #1:10
		{
			for(l in 1:5) #1:8
			{
				set.seed(243)
				a=dflist[[k]][sample(1:nrow(dflist[[k]]),floor(runif(1, 67, 70)*nrow(dflist[[k]])/100)),]
				set.seed(243)
				b=rbind(a,a[sample(1:nrow(a),max_row_num-nrow(a)),])
				b=merge(training_data,b,by=c('id','dv'))
				b=b[order(b[,'id']),]
				write.csv(b, paste("preproc_data/dfold",k,"_bag",l,".csv",sep=''),row.names=F)
				
			}
			write.csv(merge(training_data,dflist[[k]],by=c('id','dv')),paste("preproc_data/dfold",k,".csv",sep=''),row.names=F)
		}

}
	