
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
# i=2
	setwd(paste(home,ds[i,"name"],"/",sep=''))

	preproc_dir=paste(home,ds[i,"name"],"/","preproc_data",sep='')

	candidates_val=paste(home,ds[i,"name"],"/","candidates_val",sep='')

	candidates_test=paste(home,ds[i,"name"],"/","candidates_test",sep='')
	
	setwd(candidates_val)
	myFiles <- list.files(pattern="*csv")
	
	library(plyr)
	import <- llply(myFiles, read.csv)
	b=do.call("cbind", import)
	

	setwd(preproc_dir)
	c=read.csv("val.csv",as.is=T)
	b=cbind(c[,c("id","dv")],b)
	setwd(candidates_val)
	write.csv(b,"all_val.csv",row.names=F)
	col=gsub('val','test',colnames(b))
	
	
	setwd(candidates_test)
	myFiles <- list.files(pattern="*csv")

	
	import <- llply(myFiles, read.csv)
	b=do.call("cbind", import)

	setwd(preproc_dir)
	c=read.csv("test.csv",as.is=T)
	b=cbind(c[,c("id","dv")],b)
	setwd(candidates_test)
	b=b[,col]
	write.csv(b,"all_test.csv",row.names=F)
	
}
