#!/bin/bash 
# Telling how many nodes and processors should be used. 
#PBS -l nodes=7:ppn=8,walltime=240:00:00 
# Naming the file 
#PBS -N RP_sim_int10 
# Outputting error 
#PBS -j oe 
# Not sure what the two next lines do 
#PBS -q default 
#PBS -S /bin/bash 
#PBS -m e 
#PBS -M gjh27@cornell.edu

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
# All of the above 'R --vanilla...' is for the cluster 
# All of the below 'R --vanilla...' is an R file 
# This is the beginning of a 'regular' R file 
################################################################################# 

sfLibrary(MASS) 
sfLibrary(rpart) 
#sfLibrary(pracma)
sfLibrary(far)

numsim = 1000

################################################################################################################################################################## 
################################################################################################################################################################## 
######################################################### Main Function ####################################################################################### 
################################################################################################################################################################## #
#################################################################################################################################################################

RP.sim.int1 <- function(beta=0,d1=1,d0=1,n=500,g=5,r=g,K=floor(2*(sqrt(n))),nx1=50,nmc=250,nrp=1000,minsplit=3,maxcompete=0,maxsurrogate=0,usesurrogate=0) {


######################################################################## 
##################### Defining Control Parameters ###################### 
######################################################################## 

control.sim <- rpart.control(minsplit=minsplit,maxcompete=maxcompete,maxsurrogate=maxsurrogate,usesurrogate=usesurrogate) 

m <- nx1*nmc


######################################################################## 
##################### Creating Training Sets ######################### 
########################################################################


x1 <- matrix(runif(n*d1,-1,1),n,d1)
x2 <- matrix(runif(n*d1,-1,1),n,d1)
x3 <- matrix(runif(n*d0,-1,1),n,d0)
eps <- rnorm(n,sd=0.1) 

y <- beta*apply(x1,1,sum) + apply(x2,1,sum)  + eps 

df.m1 <- data.frame(x1,x2,x3,y)



######################################################################## 
 ################### Defining Test Grids ############################## 
########################################################################

# Define test.grid.3D 

 x1g <- matrix(runif(d1*g,-1,1),g,d1)[rep(1:g,g),]           # equispace grid
 x2g <- matrix(runif((d1+d0)*g,-1,1),g,d1+d0)[rep(1:g,each=g),] # random sample
 
test.grid.2D <- data.frame(x1g,x2g,rep(0,g^1))
names(test.grid.2D) = names(df.m1)



###################################################################### 
################# Defining the vimp proj. mat. D ################### 
######################################################################



D.2D <- diag(g^2) - diag(g)%x%matrix(1,g,g)/g



###################################################################### 
################# Building Trees and Predicting #################### 
###################################################################### 

R.2D <- vector("list",nrp) 
cond.exp.list.m1 <- vector("list",nrp) 
full.pred.list.m1 <- vector("list",nrp) 
tstat <- rep(0,nrp)

for (i in 1:nrp) { 	
  R.2D[[i]] <- orthonormalization(matrix(rnorm(r*g^2),nrow=g^2,ncol=r))[,1:r] 	

#  R.2D[[i]] <- gramSchmidt(matrix(rnorm(r*g^2),nrow=g^2,ncol=r))$Q  

  cond.exp.list.m1[[i]] <- matrix(rep(0,nx1*r),nrow=nx1) 	
  full.pred.list.m1[[i]] <- matrix(rep(0,r),nrow=1) 	

  cat("RandProj ",i," of ",nrp,"\n") 
} 

for (j in 1:nx1) { 	
  ind.x1 <- sample(1:n,size=1,replace=FALSE) 	
  pred.list.m1 <- vector("list",nrp) 	

  for (i in 1:nrp) { 		
    pred.list.m1[[i]] <- matrix(rep(0,nmc*r),nrow=nmc) 	
  } 	
  
  for (k in 1:nmc) { 		
    ind <- c(ind.x1,sample((1:n)[-ind.x1],K-1,replace=FALSE)) 		
    ss.m1 <- df.m1[ind,]			
    tree.m1 <- rpart(y~.,data=ss.m1,control=control.sim) 		
    pred.N.m1 <- matrix(predict(tree.m1,test.grid.2D),nrow=g^2) #1 total2d 		
    
    
    for (i in 1:nrp) { 			
      pred.list.m1[[i]][k,] <- t(D.2D%*%pred.N.m1)%*%R.2D[[i]]				
    } 		
    
    cat("nx1: ",j," nmc: ",k,"\n") 	
  } 	
  
  for (i in 1:nrp) { 		
    cond.exp.list.m1[[i]][j,] <- apply(pred.list.m1[[i]],2,mean) 		
    full.pred.list.m1[[i]] <- rbind(full.pred.list.m1[[i]],pred.list.m1[[i]])			
  } 
} 

for (i in 1:nrp) { 	
  full.pred.list.m1[[i]] <- full.pred.list.m1[[i]][-1,] 	
  mean.m1 <- matrix(apply(full.pred.list.m1[[i]],2,mean),ncol=1) 	
  tstat[i] <- t(mean.m1) %*% ginv((m/n)*((K^2)/m)*cov(cond.exp.list.m1[[i]]) + (1/m)*cov(full.pred.list.m1[[i]])) %*% mean.m1 
} 

pval <- 1 - pchisq(df=r,tstat) 
mean.pval <- mean(pval) 

return(list("tstat"=tstat,"pval"=pval,"mean.pval"=mean.pval)) 
}



################################################################################# 
# Need to export the data and functions to the worker nodes if you want them to use them 
################################################################################# 

sfExport("RP.sim.int1")



################################################################################# 
# Creating a wrapper function that runs the main function nsim times 
################################################################################# 



d0sim <- c(5,10,20)
#d0sim = 10
ld0 = length(d0sim)

d1sim <- c(3)
ld1 = length(d1sim)

betasim <- c(0,0.1,0.25,0.5,1,2)
lbeta = length(betasim)

nsim <- c(500)
ln = length(nsim)

#psim = cbind( rep(betasim,ld0*ld1*ln),
#              rep(d1sim,each=lbeta,ld0*ln),
#              rep(d0sim,each=ld1*lbeta,ln),
#              rep(nsim,each=ld1*ld0*lbeta) )
              
#colnames(psim) = c('beta','d1','d0','n')

#psim = psim[ rep(1:nrow(psim),each=numsim), ]

#psim = split(psim,row(psim))



wrapper <- function(psim){ 
  result <- RP.sim.int1(d0 = psim[2],beta=psim[1],d1=3) 
  return(result) 
}


for(i in 1:ld0){
   for(j in 1:6){
 
     psim = cbind(rep(betasim[j],numsim),rep(d0sim[i],numsim))
     colnames(psim) = c('beta','d0') 
     psim = split(psim,row(psim))


     # Running the wrapper function nSim times: 

     BigListResult = sfLapply(psim,wrapper) 

    # Save the result to a folder on the cluster 

    save(BigListResult,file = paste('Sim_Results_vimp01b',j,'d',i,'.rda',sep=''))
   }
}

# Close all connections 

sfStop() 
