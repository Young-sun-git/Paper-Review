#!/bin/bash
# Telling how many nodes and processors should be used.
#PBS -l nodes=7:ppn=8,walltime=24:00:00
# Naming the file
#PBS -N RP_sim_m1
# Outputting error
#PBS -j oe
# Not sure what the two next lines do
#PBS -q default
#PBS -S /bin/bash
#PBS -m e
#PBS -M lkm54@cornell.edu

cd $PBS_O_WORKDIR

# Telling cluster that you are using R
R --vanilla > mysnow.out <<EOF

# Looking for what machines are available to use.
library(snowfall)
# library(snow)
pbsnodefile = Sys.getenv("PBS_NODEFILE")
machines <- scan(pbsnodefile, what="")
machines
nmach = length(machines)

# Initializing the nodes
sfInit(parallel=TRUE,type='SOCK',cpus=nmach,socketHosts=machines)


#################################################################################
#  All of the above 'R --vanilla...' is for the cluster
#  All of the below 'R --vanilla...' is an R file
#  This is the beginning of a 'regular' R file
#################################################################################
sfLibrary(MASS)
sfLibrary(rpart)
sfLibrary(far)




##################################################################################################################################################################
##################################################################################################################################################################
#########################################################   Main Function  #######################################################################################
##################################################################################################################################################################
##################################################################################################################################################################

RP.sim.m1 <- function(nx1=50,nmc=250,nrp=1000,r=5,minsplit=3,maxcompete=0,maxsurrogate=0,usesurrogate=0) {


########################################################################
###################  Defining Control Parameters  ######################
########################################################################
control.sim <- rpart.control(minsplit=minsplit,maxcompete=maxcompete,maxsurrogate=maxsurrogate,usesurrogate=usesurrogate)
m <- nx1*nmc


########################################################################
###################  Defining Test Grids  ##############################
########################################################################
# Define test.grid.2D
x1 <- rep(seq(0.2,0.8,length.out=10),10)
x2 <- rep(seq(0.2,0.8,length.out=10),each=10)
test.grid.2D <- data.frame(x1,x2)

# Define test.grid.3D
x1 <- rep(seq(0.3,0.7,0.1),5^2)
x2 <- rep(rep(seq(0.3,0.7,0.1),each=5),5)
x3 <- rep(seq(0.3,0.7,0.1),each=5^2)
test.grid.3D <- data.frame(x1,x2,x3)



########################################################################
#####################  Creating Training Sets  #########################
########################################################################
### Model 1
x1 <- runif(500)
x2 <- runif(500)
x3 <- runif(500)
eps <- rnorm(500,sd=0.05)
y <- x1 + eps
df.m1 <- data.frame(x1,x2,y)



######################################################################
#################  Defining the add. proj. mat. D  ###################
######################################################################

Dbar.2D <- matrix(1/100,100,100)
Di. <- matrix(1,10,10)%x%diag(10)/10
D.j <- diag(10)%x%matrix(1,10,10)/10

Dbar.3D <- matrix(1/125,125,125)
Di.. <- matrix(1,25,25)%x%diag(5)/25
D.j. <- matrix(1,5,5)%x%diag(5)%x%matrix(1,5,5)/25
D..k <- diag(5)%x%matrix(1,25,25)/25
Di.k <- diag(5)%x%matrix(1,5,5)%x%diag(5)/5
D.jk <- diag(25)%x%matrix(1,5,5)/5

D.2D <- diag(100) - Di. - D.j + Dbar.2D
D.3D <- diag(125) - Di.. - D.j. - D..k + 2*Dbar.3D
D.part <- diag(125) - Di.k - D.jk + D..k



######################################################################
#################  Building Trees and Predicting  ####################
######################################################################
R.2D <- vector("list",nrp)
R.3D <- vector("list",nrp)
cond.exp.list.m1 <- vector("list",nrp)
full.pred.list.m1 <- vector("list",nrp)
tstat <- rep(0,nrp)

for (i in 1:nrp) {
	R.3D[[i]] <- orthonormalization(matrix(rnorm(125^2),nrow=125))[,1:r]
	R.2D[[i]] <- R.3D[[i]][1:100,]
	cond.exp.list.m1[[i]] <- matrix(rep(0,nx1*r),nrow=nx1)
	full.pred.list.m1[[i]] <- matrix(rep(0,r),nrow=1)
	cat("RandProj  ",i,"  of  ",nrp,"\n")
}
for (j in 1:nx1) {
	ind.x1 <- sample(1:500,size=1,replace=FALSE)
	pred.list.m1 <- vector("list",nrp)
	for (i in 1:nrp) {
		pred.list.m1[[i]] <- matrix(rep(0,nmc*r),nrow=nmc)
	}
	for (k in 1:nmc) {
		ind <- c(ind.x1,sample((1:500)[-ind.x1],49,replace=FALSE))
		ss.m1 <- df.m1[ind,]	
		tree.m1 <- rpart(y~.,data=ss.m1,control=control.sim)
		pred.N.m1 <- matrix(predict(tree.m1,test.grid.2D),nrow=100) #1  total2d
		for (i in 1:nrp) {
			pred.list.m1[[i]][k,] <- t(D.2D%*%pred.N.m1)%*%R.2D[[i]]		
		}
		cat("nx1:  ",j,"          nmc:  ",k,"\n")
	}
	for (i in 1:nrp) {
		cond.exp.list.m1[[i]][j,] <- apply(pred.list.m1[[i]],2,mean)
		full.pred.list.m1[[i]] <- rbind(full.pred.list.m1[[i]],pred.list.m1[[i]])		
	}
}
for (i in 1:nrp) {
	full.pred.list.m1[[i]] <- full.pred.list.m1[[i]][-1,]
	mean.m1 <- matrix(apply(full.pred.list.m1[[i]],2,mean),ncol=1)
	tstat[i] <- t(mean.m1) %*% ginv((m/500)*((50^2)/m)*cov(cond.exp.list.m1[[i]]) + (1/m)*cov(full.pred.list.m1[[i]])) %*% mean.m1
}
pval <- 1 - pchisq(df=r,tstat)
mean.pval <- mean(pval)
return(list("tstat"=tstat,"pval"=pval,"mean.pval"=mean.pval))
}



#################################################################################
#  Need to export the data and functions to the worker nodes if you want them to use them
#################################################################################
sfExport("RP.sim.m1")



#################################################################################
# Creating a wrapper function that runs the main function nsim times
#################################################################################
wrapper <- function(nsim){
result <- RP.sim.m1(r=5)
return(result)
}

nSim <- 1000

# Running the wrapper function nSim times:
BigListResult = sfLapply(1:nSim,wrapper)
# Save the result to a folder on the cluster
save(BigListResult,file = '~/RandProj/Sims/Sim_Results_m1.rda')

# Close all connections
sfStop()