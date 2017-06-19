
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

	if(i<3)
	{dat=read.table(ds[i,7], header=FALSE)
	}else{
	dat=read.csv(ds[i,7], as.is=T)}


	for(j in 1:ncol(dat))
	{
	colnames(dat)[j]<-paste(j,substring(ds[i,"name"],1,3),sep='')
	}
	colnames(dat)[ds[i,"dv_col"]]<-paste('dv')

	dir.create(paste(home,ds[i,"name"],sep=''))

	setwd(paste(home,ds[i,"name"],"/",sep=''))

	dir.create("preproc_data")

	 
	dat$id=rownames(dat)


	## Flags creation for categorical dataset with more than 2 values
	cat<-substr(ds[i,"cat"],2,nchar(ds[i,"cat"])-1)
	cat_var<- as.numeric(unlist(strsplit(cat, " ")))
	dat_cat=dat[,cat_var]


	cont<-substr(ds[i,"cont"],2,nchar(ds[i,"cont"])-1)
	cont_var<- as.numeric(unlist(strsplit(cont, " ")))
	dat_cont=dat[,cont_var]

	dat_dv=dat[,c(ncol(dat),ds[i,"dv_col"])]


	##Splitting into val, test and train

	##Creating Local Test data set for model validation purposes
	library(caret)
	set.seed(3456)
	#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
	devIndex <- as.data.frame(createFolds(dat_dv$dv, k=5,  list = TRUE))

	data_div_1<- dat_dv[ devIndex$Fold1, c("id","dv")]
	data_div_2<- dat_dv[ devIndex$Fold2, c("id","dv")]
	data_div_3<- dat_dv[ devIndex$Fold3, c("id","dv")]
	data_div_4<- dat_dv[ devIndex$Fold4, c("id","dv")]
	data_div_5<- dat_dv[ devIndex$Fold5, c("id","dv")]


	md_rows <- rbind(data_div_1,data_div_2,data_div_3)
	val_rows <- data_div_4
	test_rows <- data_div_5

	data_md = merge( dat_dv , md_rows,by=c('id','dv'))
	data_val = merge( dat_dv , val_rows,by=c('id','dv'))
	data_test = merge( dat_dv , test_rows,by=c('id','dv'))

	data_md$sample="train"
	data_val$sample="val"
	data_test$sample='test'


	full_dv_data= rbind(data_md[,sort(colnames(data_md))], data_val[,sort(colnames(data_val))], data_test[,sort(colnames(data_test))])
	class(full_dv_data$id)


	full_dv_data$id=as.numeric(full_dv_data$id)
	class(full_dv_data$id)
	head(full_dv_data)


	full_dv_data=full_dv_data[order(full_dv_data[,'id']),]

	dim(full_dv_data)

	write.csv(full_dv_data,"preproc_data/dv.csv",row.names=F)
	dat_dv=full_dv_data

	######

	if (ncol(dat_cat)>0)
	{
	category_data=dat_cat


	categorical_idvs=colnames(category_data)

	#function for finding the mode
	Mode <- function(x) {
	  ux <- unique(x)
	  ux[which.max(tabulate(match(x, ux)))]
	}


	#using the above function and finding mode for each variable
	data_cat_mode=list(category_data)


	for(x in 1:ncol(category_data))
	{
	data_cat_mode[[x]]=Mode(category_data[,x])
	}
	names(data_cat_mode)=categorical_idvs


	data_cat_mode1=as.data.frame(data_cat_mode,stringsAsFactors=FALSE)
	data_cat_mode1[data_cat_mode1 == ""] <-  "IsNull"
	colnames(data_cat_mode1)<-paste(gsub("X","",colnames(data_cat_mode1)))

	#Replacing all left out NAs with mode of respective variables.
	for(idv in colnames(dat_cat)) category_data[is.na(category_data[,idv]),idv] =(data_cat_mode1[,idv])




	if(ds[i,"name"]=="german") {

	a <- sapply(category_data, is.factor)
	category_data[a] <- lapply(category_data[a], as.character)

	}else 
	{

	i <- sapply(category_data,function(x) length(unique(x))>2)

	category_data[i] <- lapply(category_data[i], as.character)
	category_data[i] <- lapply(category_data[i], as.character)
	}

	library(caret)
	dummies <- dummyVars( ~ ., data = category_data)
	category_data_dummies=as.data.frame(predict(dummies, newdata = category_data))
	dim(category_data_dummies)
	head(category_data_dummies)

	for(k in 1:length(colnames(category_data_dummies)))
	{
		colnames(category_data_dummies)[i]<-gsub('`','_',colnames(category_data_dummies)[i])
	}

	head(category_data_dummies)

	category_data_final=cbind(dat_cat,category_data_dummies)

	write.csv(category_data_final,"preproc_data/cat_data.csv",row.names=F)

	categorical_idvs_final=colnames(category_data_final)

	write.csv(categorical_idvs_final,"preproc_data/cat_names.csv",row.names=F)

	}



	#### Bin Creation

	## IGNORING OUTLIER DETECTION 

	if (ncol(dat_cont)>0)
	{
	continuous_data_originalvar=dat_cont

	cont_idvs_toimpute=colnames(continuous_data_originalvar)
	means = colMeans(continuous_data_originalvar[,cont_idvs_toimpute],na.rm=T)
	for(idv in cont_idvs_toimpute) continuous_data_originalvar[is.na(continuous_data_originalvar[,idv]),idv] =means[idv]


	breaks=list(continuous_data_originalvar)
	for(m in 1:ncol(continuous_data_originalvar)) 
	{
	breaks=unique(quantile(continuous_data_originalvar[,m], probs = seq(0, 1, by= 0.1)))
	continuous_data_originalvar[,paste(colnames(continuous_data_originalvar[m]),"bin",sep="_")] <- cut(continuous_data_originalvar[,m], breaks,include.lowest=TRUE ,labels=c(1:ifelse(length(breaks)>1,(length(breaks) - 1),length(breaks))))
	}
	# continuous_data_originalvar$customer_id_bin<-NULL


	 
	continuous_data_final= continuous_data_originalvar


	dim(continuous_data_final)


	write.csv(continuous_data_final,"preproc_data/cont.csv",row.names=F)

	continuous_idvs_final=colnames(continuous_data_final)

	write.csv(continuous_idvs_final,"preproc_data/cont_names.csv",row.names=F)

	}


	###FULL_DATA_MERGING

	if (ncol(dat_cat)>0)
	{
	category_data_final = read.csv("preproc_data/cat_data.csv",as.is=T)
	}

	if (ncol(dat_cont)>0)
	{
	continuous_data_final= read.csv ("preproc_data/cont.csv",as.is=T)
	}


	res <- try(dim(category_data_final),silent = TRUE)
	res2 <- try(dim(continuous_data_final),silent = TRUE)
	if(class(res) == "try-error")
	{data_final=continuous_data_final}

	if(class(res2) == "try-error")
	{data_final=category_data_final}

	if(class(res2) != "try-error" & class(res) != "try-error")
	{
	data_final=cbind(category_data_final,continuous_data_final)
	}

	data_final=cbind(dat_dv,data_final)
	data_final=data_final[,c("id","dv",setdiff(colnames(data_final),c("id","dv")))]
	write.csv(data_final,"preproc_data/full_prep.csv",row.names=F)


	#removing 0 variance predictors
	data_final <- data_final[sapply(data_final, function(x) length(levels(factor(x,exclude=NULL)))>1)]

	nzv<-nearZeroVar(data_final)
	if(length(nzv)>0) data_final=data_final[,-nzv]

	data_final_train=data_final[data_final$sample=='train',c(1,2,4:length(colnames(data_final)))]
	data_final_val=data_final[data_final$sample=='val',c(1,2,4:length(colnames(data_final)))]
	data_final_test=data_final[data_final$sample=='test',c(1,2,4:length(colnames(data_final)))]

	write.csv(data_final_train,"preproc_data/full_train.csv",row.names=F)
	write.csv(data_final_val,"preproc_data/val.csv",row.names=F)
	write.csv(data_final_test,"preproc_data/test.csv",row.names=F)

	list2=ls()
	list2=setdiff(list2,c("rows","ds","home","user","Desktop"))
	rm(list=list2)
	setwd(home)

}







