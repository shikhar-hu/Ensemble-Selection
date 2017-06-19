


GeneratePopulation <- function(popsize,dim,low,up)
{
	pop = matrix(1,popsize,dim)

	for (i in 1:popsize)
	{
		for (j in 1:dim)
		{
			pop[i,j]=runif(1,0,1)*(up[j]-low[j])+low[j]
		}
	}
	pop
}


library(compiler)

GenPop <- cmpfun(GeneratePopulation)


BoundaryControl <- function (mat)
{
mat[mat < 0] <- 0
mat[mat >1] <- 1
mat
}  



library(ROCR)
feval<-function (fnc,pop,mydata)
{
	sum_mat=sum(pop)
	check=as.data.frame(cbind(mydata[,c("id","dv")], comb_pred=rowSums( mapply('*', mydata[,-(1:2)],c(t(pop))))/sum_mat))
	pred <- prediction( check$comb_pred, check$dv)
	fnc.tmp <- performance(pred,fnc) 
	fncout <- as.numeric(fnc.tmp@y.values)
	fncout
}


library(compiler)

fncval <- cmpfun(feval)

library(R.utils) 




bsa=function(fnc,mydata,popsize,dim,DIM_RATE,low,up,n_iter)
{
	library(ROCR)

	# #INITIALIZATION


	if (length(up)==1)	 up = sample(up,dim,replace=T)
	if (length(low)==1)		low = sample(low,dim,replace=T)  

	pop=GenPop(popsize,dim,low,up)  #see Eq.1 in [1]
	fitnesspop=fncval(fnc,pop,mydata) 
	historical_pop=GenPop(popsize,dim,low,up) # see Eq.2 in [1]
    all_improv <- list()
	all_improv[[1]]<-fitnesspop
	out <- data.frame(Run=numeric(), AUC=numeric(), Runtime=numeric()) 
	pb <- winProgressBar(title = "BSA Checking Runtime", min = 0, max = n_iter, width = 400)
	
	# historical_pop  is swarm-memory of BSA as mentioned in [1].

	# ------------------------------------------------------------------------------------------ 
	for (epk in 1:n_iter)
	{
	
		#SELECTION-I
		start.time <- Sys.time()


		if (runif(1,0,1)<runif(1,0,1)) historical_pop=pop # see Eq.3 in [1]
		historical_pop=historical_pop[sample(popsize),] # see Eq.4 in [1]
		F=runif(1,0,1)*3 # see Eq.5 in [1], you can other F generation strategies 
		map=matrix(0,popsize,dim) #zeros(popsize,dim) # see Algorithm-2 in [1]         
		if (runif(1,0,1)<runif(1,0,1))
			for (i in 1:popsize)
			{
				u=sample(dim) 
				map[i,u[1:ceiling(DIM_RATE*runif(1,0,1)*dim)]]=1  ##Shikhar NOTE - DIM_RATE is speed of backsearch? 1 or less
			} else 
		{
			for (i in 1:popsize)
			{
				map[i,sample(dim)[1]]=1
			}
		}
				
		
		# RECOMBINATION (MUTATION+CROSSOVER)   
		offsprings=pop+(map*F)*(historical_pop-pop)  # see Eq.5 in [1]    
		offsprings=BoundaryControl(offsprings) # see Algorithm-3 in [1]
		
		# SELECTON-II
		fitnessoffsprings=fncval(fnc,offsprings,mydata)
		
		if(fitnessoffsprings-fitnesspop >= 0.0001 )	
		{
			fitnesspop =fitnessoffsprings
			pop=offsprings
			n=length(all_improv)
			all_improv[[n+1]]<-pop
			# print(paste("epoch=",epk,"; AUC=",fitnessoffsprings,sep=""))
			# flush.console()

		}
		globaloptimum =fitnesspop
		globaloptimizer=pop
		# EXPORT SOLUTIONS 
		end.time <- Sys.time() 
		Runtime=as.numeric(end.time - start.time)

		out[epk,]=c(epk,fitnesspop ,Runtime)
		setWinProgressBar(pb, epk, title=paste( round(epk/n_iter*100, 0),"% done - Checking Runtime"))
		
		
	}
	close(pb)
write.csv(out, paste("bsa_outlog_",DIM_RATE*100,"_",format(Sys.time(),  "%Y%m%d_%H"),".csv", sep=""), row.names=FALSE)
saveObject(all_improv, paste("bsa_improv_",DIM_RATE*100,"_",format(Sys.time(),  "%Y%m%d_%H"),".Rbin", sep="")) 

globaloptimizer

}
