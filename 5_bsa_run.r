
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
	mydata = read.csv("all_val.csv",as.is=T)


	a=ncol(mydata)
	a=a-2
	fact <- function(x) {
		x <- as.integer(x)
		div <- seq_len(abs(x))
		factors <- div[x %% div == 0L]
		factors <- list(neg = -factors, pos = factors)
		return(factors)
	}

	if(length(fact(a)$pos)==2)
	{a=a-1
	mydata=mydata[,-201]}

	factors=fact(a)$pos
	if (a %% 2==0)
	{popsize=factors[length(factors)/2]
	dim=factors[length(factors)/2+1]
	}else{
	popsize=factors[length(factors)/2]
	dim=factors[length(factors)/2]
	}


	a==dim*popsize

	low=0
	up=1
	mydata=mydata

	DIM_RATE=0.6

	fnc="auc"


	####CHECK Time for running of 10 iterations
	setwd(home)

	source("bsa_function.r")
	setwd(paste(home,ds[i,"name"],"/",sep=''))

	dir.create("ensemble_check_big")

	ensemble_check=paste(home,ds[i,"name"],"/","ensemble_check_big",sep='')

	setwd(ensemble_check)

	  
	n_iter=10
	start_check <- Sys.time()

	bsa_result=bsa(fnc,mydata,popsize,dim,DIM_RATE,low,up,n_iter)

	stop_check <- Sys.time()
	timetaken=as.numeric(stop_check-start_check,units="secs")
	n_iter_1sec=n_iter/timetaken

	start_all <- Sys.time()


	###Running the BSA
	setwd(home)


	source("bsa_function.r") 
	 
	n_iter=floor(n_iter_1sec*3600*4)

	setwd(ensemble_results)

	bsa_result=bsa(fnc,mydata,popsize,dim,DIM_RATE,low,up,n_iter,ds,i)

	currentBest=feval(fnc,bsa_result,mydata)

	setwd(ensemble_results)

	all_improv <- loadObject(paste("bsa_improv_",DIM_RATE*100,"_",format(Sys.time(), "%Y%m%d_%H"),".Rbin", sep="")) 
	out=read.csv(paste("bsa_outlog_",DIM_RATE*100,"_",format(Sys.time(), "%Y%m%d_%H"),".csv", sep=""),as.is=T)

	setwd(candidates_test)

	testdata = read.csv("all_test.csv",as.is=T)
	b=ncol(testdata)
	if(length(fact(b)$pos)==2)
	{testdata=testdata[,-201]}

	testBest=feval(fnc,bsa_result,testdata)

	test=c()
	for(m in 1:length(all_improv))
	{
	test[m]=feval(fnc,all_improv[[m]],testdata)
	}

	# val_check=c()
	# for(m in 1:length(all_improv))
	# {
	# val_check[m]=feval(fnc,all_improv[[m]],mydata)
	# }


	val_improv=out[!duplicated(out$AUC),]

	if (length(test)-1==nrow(val_improv)){
	all_output=cbind(val_improv,AUC_test=test[-1])
	}else{all_output=cbind(val_improv,AUC_test=test)}


	setwd(ensemble_results)

	write.csv(all_output,paste("BSA_output_",DIM_RATE*100,"_",format(Sys.time(), "%Y%m%d_%H"),".csv", sep=""),row.names=F)

	stop_all <- Sys.time()
	timetaken=as.numeric(stop_all-start_all,units="secs")


	a=matrix(c(DIM_RATE,n_iter,timetaken),1)
	colnames(a)<-paste(c("DIM_RATE","n_iter","timetaken"))

	write.csv(t(a), paste("bsa_timetaken_",DIM_RATE*100,"_",format(Sys.time(),"%Y%m%d_%H"),".csv", sep=""), row.names=T)

}