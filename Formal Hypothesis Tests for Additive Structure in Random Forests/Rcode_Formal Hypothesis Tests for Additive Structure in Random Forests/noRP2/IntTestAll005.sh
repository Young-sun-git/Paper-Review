#!/bin/bash
# Telling how many nodes and processors should be used.
#PBS -l nodes=12:ppn=8,walltime=72:00:00
# Naming the file
#PBS -N Int_New
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
#  All of the above 'R --vanilla...' is for the cluster
#  All of the below 'R --vanilla...' is an R file
#  This is the beginning of a 'regular' R file
#################################################################################
sfLibrary(MASS)
sfLibrary(rpart)



#################################################################################
#  Main function
#################################################################################

IntTestAll <- function(nx1=50,nmc=250,minsplit=3,maxcompete=0,maxsurrogate=0,usesurrogate=0) {


tstat.m2.500 <- 0
tstat.m3.500 <- 0
tstat.m5.500 <- 0
tstat.m7.500 <- 0
tstat.m8.500 <- 0
tstat.m10.500.part <- 0
tstat.m11.500.part <- 0
tstat.m13.500 <- 0
tstat.m14.500.part <- 0
tstat.m15.500 <- 0
tstat.m17.500 <- 0
tstat.m18.500.part <- 0


tstat.m19.500 <- 0

tstat.m20.500.part <- 0

# ######################################################################
# #####################  Defining Control Parameters  ##################
# ######################################################################

control.sim <- rpart.control(minsplit=minsplit,maxcompete=maxcompete,maxsurrogate=maxsurrogate,usesurrogate=usesurrogate)


######################################################################
#####################  Creating Test Grids  ##########################
######################################################################

# Define test.grid.2D
x1.grid <- rep(seq(0.2,0.8,0.2),4)
x2.grid <- rep(seq(0.2,0.8,0.2),each=4)
test.grid.2D <- data.frame("x1"=x1.grid,"x2"=x2.grid)

# Define test.grid.3D
x1.grid <- rep(c(0.3,0.5,0.7),9)
x2.grid <- rep(rep(c(0.3,0.5,0.7),each=3),3)
x3.grid <- rep(c(0.3,0.5,0.7),each=9)
test.grid.3D <- data.frame("x1"=x1.grid,"x2"=x2.grid,"x3"=x3.grid)


######################################################################
#####################  Creating Datasets  ############################
######################################################################



# m2:  y = x1
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500 + eps.500
df.m2.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# m3:  y = exp(x1)
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + eps.500
df.m3.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)


# m5:  y = exp(x1) + sin(pi x2)
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + sin(pi*x2.500) + eps.500
df.m5.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)


# m7:  y = x1 + x2 + x3
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500 + x2.500 + x3.500 + eps.500
df.m7.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# m8:  y = exp(x1) + exp(x2) + exp(x3)
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + exp(x2.500) + exp(x3.500) + eps.500
df.m8.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)


# m10:  y = x1*x3 + x2*x3
 x1.500 <- runif(500)
 x2.500 <- runif(500)
 x3.500 <- runif(500)
 eps.500 <- rnorm(500,0,sd=0.05)
 y.500 <- x1.500*x3.500 + x2.500*x3.500 + eps.500
 df.m10.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# # m11:  y = exp(x1*x3) + exp(x2*x3)
 x1.500 <- runif(500)
 x2.500 <- runif(500)
 x3.500 <- runif(500)
 eps.500 <- rnorm(500,0,sd=0.05)
 y.500 <- exp(x1.500*x3.500) + exp(x2.500*x3.500) + eps.500
 df.m11.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)


# m13:  y = x1*x2
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500*x2.500 + eps.500
df.m13.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# m14:  y = x1*x2*x3
 x1.500 <- runif(500)
 x2.500 <- runif(500)
 x3.500 <- runif(500)
 eps.500 <- rnorm(500,0,sd=0.05)
 y.500 <- x1.500*x2.500*x3.500 + eps.500
 df.m14.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# m15:  y = exp(5*(x1+x2))/(1+exp(5*(x1+x2))) - 1
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(5*(x1.500+x2.500))/(1+exp(5*(x1.500+x2.500))) - 1 + eps.500
df.m15.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)


# m17:  y = 0.5*(1+sin(2*pi*(x1+x2)))
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- 0.5*(1+sin(2*pi*(x1.500+x2.500))) + eps.500
df.m17.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# m18:  y = 0.5*(1+sin(2*pi*(x1+x2+x3)))
 x1.500 <- runif(500)
 x2.500 <- runif(500)
 x3.500 <- runif(500)
 eps.500 <- rnorm(500,0,sd=0.05)
 y.500 <- 0.5*(1+sin(2*pi*(x1.500+x2.500+x3.500))) + eps.500
 df.m18.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# m19:  y = (64*(x1*x2)^3)*(1-x1*x2)^3
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- (64*(x1.500*x2.500)^3)*(1-x1.500*x2.500)^3 + eps.500
df.m19.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# # m20:  y = (64*(x1*x2*x3)^3)*(1-x1*x2*x3)^3
 x1.500 <- runif(500)
 x2.500 <- runif(500)
 x3.500 <- runif(500)
 eps.500 <- rnorm(500,0,sd=0.05)
 y.500 <- (64*(x1.500*x2.500*x3.500)^3)*(1-x1.500*x2.500*x3.500)^3 + eps.500
 df.m20.500 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)


cond.exp.m2.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m3.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m5.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m7.500 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.m8.500 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.m10.500.part <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.m11.500.part <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.m13.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m14.500.part <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)

cond.exp.m15.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m17.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m18.500.part <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)

cond.exp.m19.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.m20.500.part <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)




full.pred.m2.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m3.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m5.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m7.500 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m8.500 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m10.500.part <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m11.500.part <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m13.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m14.500.part <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m15.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m17.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m18.500.part <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.m19.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.m20.500.part <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)

######################################################################
#################  Building Trees and Predicting  ####################
######################################################################

for (i in 1:nx1) {


ind.x1.500 <- sample(1:500,size=1,replace=FALSE)

pred.m2.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m3.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m5.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m7.500 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m8.500 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m10.500.part <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m11.500.part <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m13.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m14.500.part <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m15.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m17.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m18.500.part <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
pred.m19.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
pred.m20.500.part <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)

for (j in 1:nmc) {

ind.500 <- c(ind.x1.500,sample((1:500)[-ind.x1.500],49,replace=FALSE))

ss.m2.500 <- df.m2.500[ind.500,]
ss.m3.500 <- df.m3.500[ind.500,]
ss.m5.500 <- df.m5.500[ind.500,]
ss.m7.500 <- df.m7.500[ind.500,]
ss.m8.500 <- df.m8.500[ind.500,]
ss.m10.500 <- df.m10.500[ind.500,]
ss.m11.500 <- df.m11.500[ind.500,]
ss.m13.500 <- df.m13.500[ind.500,]
ss.m14.500 <- df.m14.500[ind.500,]
ss.m15.500 <- df.m15.500[ind.500,]
ss.m17.500 <- df.m17.500[ind.500,]
ss.m18.500 <- df.m18.500[ind.500,]
ss.m19.500 <- df.m19.500[ind.500,]
ss.m20.500 <- df.m20.500[ind.500,]


tree.m2.500 <- rpart(y~.,data=ss.m2.500,control=control.sim)
tree.m3.500 <- rpart(y~.,data=ss.m3.500,control=control.sim)
tree.m5.500 <- rpart(y~.,data=ss.m5.500,control=control.sim)
tree.m7.500 <- rpart(y~.,data=ss.m7.500,control=control.sim)
tree.m8.500 <- rpart(y~.,data=ss.m8.500,control=control.sim)
tree.m10.500 <- rpart(y~.,data=ss.m10.500,control=control.sim)
tree.m11.500 <- rpart(y~.,data=ss.m11.500,control=control.sim)
tree.m13.500 <- rpart(y~.,data=ss.m13.500,control=control.sim)
tree.m14.500 <- rpart(y~.,data=ss.m14.500,control=control.sim)
tree.m15.500 <- rpart(y~.,data=ss.m15.500,control=control.sim)
tree.m17.500 <- rpart(y~.,data=ss.m17.500,control=control.sim)
tree.m18.500 <- rpart(y~.,data=ss.m18.500,control=control.sim)
tree.m19.500 <- rpart(y~.,data=ss.m19.500,control=control.sim)
tree.m20.500 <- rpart(y~.,data=ss.m20.500,control=control.sim)


pred.m2.500[j,] <- predict(tree.m2.500,test.grid.2D)
pred.m3.500[j,] <- predict(tree.m3.500,test.grid.2D)
pred.m5.500[j,] <- predict(tree.m5.500,test.grid.2D)
pred.m7.500[j,] <- predict(tree.m7.500,test.grid.3D)
pred.m8.500[j,] <- predict(tree.m8.500,test.grid.3D)
pred.m10.500.part[j,] <- predict(tree.m10.500,test.grid.3D)
pred.m11.500.part[j,] <- predict(tree.m11.500,test.grid.3D)
pred.m13.500[j,] <- predict(tree.m13.500,test.grid.2D)
pred.m14.500.part[j,] <- predict(tree.m14.500,test.grid.3D)
pred.m15.500[j,] <- predict(tree.m15.500,test.grid.2D)
pred.m17.500[j,] <- predict(tree.m17.500,test.grid.2D)
pred.m18.500.part[j,] <- predict(tree.m18.500,test.grid.3D)
pred.m19.500[j,] <- predict(tree.m19.500,test.grid.2D)
pred.m20.500.part[j,] <- predict(tree.m20.500,test.grid.3D)

cat("nx1:  ",i,"        nmc: ",j,"\n")

}


cond.exp.m2.500[i,] <- apply(pred.m2.500,2,mean)
cond.exp.m3.500[i,] <- apply(pred.m3.500,2,mean)
cond.exp.m5.500[i,] <- apply(pred.m5.500,2,mean)
cond.exp.m7.500[i,] <- apply(pred.m7.500,2,mean)
cond.exp.m8.500[i,] <- apply(pred.m8.500,2,mean)
cond.exp.m10.500.part[i,] <- apply(pred.m10.500.part,2,mean)
cond.exp.m11.500.part[i,] <- apply(pred.m11.500.part,2,mean)
cond.exp.m13.500[i,] <- apply(pred.m13.500,2,mean)
cond.exp.m14.500.part[i,] <- apply(pred.m14.500.part,2,mean)
cond.exp.m15.500[i,] <- apply(pred.m15.500,2,mean)
cond.exp.m17.500[i,] <- apply(pred.m17.500,2,mean)
cond.exp.m18.500.part[i,] <- apply(pred.m18.500.part,2,mean)
cond.exp.m19.500[i,] <- apply(pred.m19.500,2,mean)
cond.exp.m20.500.part[i,] <- apply(pred.m20.500.part,2,mean)


full.pred.m2.500 <- rbind(full.pred.m2.500,pred.m2.500)
full.pred.m3.500 <- rbind(full.pred.m3.500,pred.m3.500)
full.pred.m5.500 <- rbind(full.pred.m5.500,pred.m5.500)
full.pred.m7.500 <- rbind(full.pred.m7.500,pred.m7.500)
full.pred.m8.500 <- rbind(full.pred.m8.500,pred.m8.500)
full.pred.m10.500.part <- rbind(full.pred.m10.500.part,pred.m10.500.part)
full.pred.m11.500.part <- rbind(full.pred.m11.500.part,pred.m11.500.part)
full.pred.m13.500 <- rbind(full.pred.m13.500,pred.m13.500)
full.pred.m14.500.part <- rbind(full.pred.m14.500.part,pred.m14.500.part)
full.pred.m15.500 <- rbind(full.pred.m15.500,pred.m15.500)
full.pred.m17.500 <- rbind(full.pred.m17.500,pred.m17.500)
full.pred.m18.500.part <- rbind(full.pred.m18.500.part,pred.m18.500.part)
full.pred.m19.500 <- rbind(full.pred.m19.500,pred.m19.500)
full.pred.m20.500.part <- rbind(full.pred.m20.500.part,pred.m20.500.part)
}



full.pred.m2.500 <- full.pred.m2.500[-1,]
full.pred.m3.500 <- full.pred.m3.500[-1,]
full.pred.m5.500 <- full.pred.m5.500[-1,]
full.pred.m7.500 <- full.pred.m7.500[-1,]
full.pred.m8.500 <- full.pred.m8.500[-1,]
full.pred.m10.500.part <- full.pred.m10.500.part[-1,]
full.pred.m11.500.part <- full.pred.m11.500.part[-1,]
full.pred.m13.500 <- full.pred.m13.500[-1,]
full.pred.m14.500.part <- full.pred.m14.500.part[-1,]
full.pred.m15.500 <- full.pred.m15.500[-1,]
full.pred.m17.500 <- full.pred.m17.500[-1,]
full.pred.m18.500.part <- full.pred.m18.500.part[-1,]
full.pred.m19.500 <- full.pred.m19.500[-1,]
full.pred.m20.500.part <- full.pred.m20.500.part[-1,]


mean.m2.500 <- apply(full.pred.m2.500,2,mean)
mean.m3.500 <- apply(full.pred.m3.500,2,mean)
mean.m5.500 <- apply(full.pred.m5.500,2,mean)
mean.m7.500 <- apply(full.pred.m7.500,2,mean)
mean.m8.500 <- apply(full.pred.m8.500,2,mean)
mean.m10.500.part <- apply(full.pred.m10.500.part,2,mean)
mean.m11.500.part <- apply(full.pred.m11.500.part,2,mean)
mean.m13.500 <- apply(full.pred.m13.500,2,mean)
mean.m14.500.part <- apply(full.pred.m14.500.part,2,mean)
mean.m15.500 <- apply(full.pred.m15.500,2,mean)
mean.m17.500 <- apply(full.pred.m17.500,2,mean)
mean.m18.500.part <- apply(full.pred.m18.500.part,2,mean)
mean.m19.500 <- apply(full.pred.m19.500,2,mean)
mean.m20.500.part <- apply(full.pred.m20.500.part,2,mean)


########################################################################
#####################  Defining the D Matrices  ########################
########################################################################

Dbar.2D <- matrix(1/16,16,16)
Di. <- matrix(1,4,4)%x%diag(4)/4
D.j <- diag(4)%x%matrix(1,4,4)/4
Dbar.3D <- matrix(1/27,27,27)
Di.. <- matrix(1,9,9)%x%diag(3)/9
D.j. <- matrix(1,3,3)%x%diag(3)%x%matrix(1,3,3)/9
D..k <- diag(3)%x%matrix(1,9,9)/9
Di.k <- diag(3)%x%matrix(1,3,3)%x%diag(3)/3
D.jk <- diag(9)%x%matrix(1,3,3)/3

D.2D <- diag(16) - Di. - D.j + Dbar.2D
D.3D <- diag(27) - Di.. - D.j. - D..k + 2*Dbar.3D
D.part <- diag(27) - Di.k - D.jk + D..k



DV.m2.500 <- D.2D %*% mean.m2.500
DV.m3.500 <- D.2D %*% mean.m3.500
DV.m5.500 <- D.2D %*% mean.m5.500

DV.m7.500 <- D.3D %*% mean.m7.500
DV.m8.500 <- D.3D %*% mean.m8.500

DV.m10.500.part <- D.part %*% mean.m10.500.part
DV.m11.500.part <- D.part %*% mean.m11.500.part

DV.m13.500 <- D.2D %*% mean.m13.500
DV.m14.500.part <- D.part %*% mean.m14.500.part

DV.m15.500 <- D.2D %*% mean.m15.500

DV.m17.500 <- D.2D %*% mean.m17.500
DV.m18.500.part <- D.part %*% mean.m18.500.part

DV.m19.500 <- D.2D %*% mean.m19.500
DV.m20.500.part <- D.part %*% mean.m20.500.part


########################################################################
#################  Calculating the Test Statistics  ####################
########################################################################

m <- nx1*nmc

tstat.m2.500 <- t(DV.m2.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m2.500) + (1/m)*cov(full.pred.m2.500)) %*% t(D.2D)) %*% DV.m2.500
tstat.m3.500 <- t(DV.m3.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m3.500) + (1/m)*cov(full.pred.m3.500)) %*% t(D.2D)) %*% DV.m3.500
tstat.m5.500 <- t(DV.m5.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m5.500) + (1/m)*cov(full.pred.m5.500)) %*% t(D.2D)) %*% DV.m5.500

tstat.m7.500 <- t(DV.m7.500) %*% ginv(D.3D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m7.500) + (1/m)*cov(full.pred.m7.500)) %*% t(D.3D)) %*% DV.m7.500
tstat.m8.500 <- t(DV.m8.500) %*% ginv(D.3D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m8.500) + (1/m)*cov(full.pred.m8.500)) %*% t(D.3D)) %*% DV.m8.500

tstat.m10.500.part <- t(DV.m10.500.part) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.m10.500.part) + (1/m)*cov(full.pred.m10.500.part)) %*% t(D.part)) %*% DV.m10.500.part
tstat.m11.500.part <- t(DV.m11.500.part) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.m11.500.part) + (1/m)*cov(full.pred.m11.500.part)) %*% t(D.part)) %*% DV.m11.500.part

tstat.m13.500 <- t(DV.m13.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m13.500) + (1/m)*cov(full.pred.m13.500)) %*% t(D.2D)) %*% DV.m13.500
tstat.m14.500.part <- t(DV.m14.500.part) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.m14.500.part) + (1/m)*cov(full.pred.m14.500.part)) %*% t(D.part)) %*% DV.m14.500.part

tstat.m15.500 <- t(DV.m15.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m15.500) + (1/m)*cov(full.pred.m15.500)) %*% t(D.2D)) %*% DV.m15.500

tstat.m17.500 <- t(DV.m17.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m17.500) + (1/m)*cov(full.pred.m17.500)) %*% t(D.2D)) %*% DV.m17.500
tstat.m18.500.part <- t(DV.m18.500.part) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.m18.500.part) + (1/m)*cov(full.pred.m18.500.part)) %*% t(D.part)) %*% DV.m18.500.part

tstat.m19.500 <- t(DV.m19.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.m19.500) + (1/m)*cov(full.pred.m19.500)) %*% t(D.2D)) %*% DV.m19.500
tstat.m20.500.part <- t(DV.m20.500.part) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.m20.500.part) + (1/m)*cov(full.pred.m20.500.part)) %*% t(D.part)) %*% DV.m20.500.part

return(list("tstat.m2.500"=tstat.m2.500,
            "tstat.m3.500"=tstat.m3.500,
            "tstat.m5.500"=tstat.m5.500,
            "tstat.m7.500"=tstat.m7.500,
            "tstat.m8.500"=tstat.m8.500,
            "tstat.m10.500"=tstat.m10.500.part,
            "tstat.m11.500"=tstat.m11.500.part,
            "tstat.m13.500"=tstat.m13.500,
            "tstat.m14.500"=tstat.m14.500.part,
            "tstat.m15.500"=tstat.m15.500,
            "tstat.m17.500"=tstat.m17.500,
            "tstat.m18.500"=tstat.m18.500.part,
            "tstat.m19.500"=tstat.m19.500,
            "tstat.m20.500"=tstat.m20.500.part))

}




#################################################################################
#  Need to export the functions to the worker nodes if you want them to use them
#################################################################################
sfExport("IntTestAll")

# Creating a wrapper function that runs the main function nsim times
wrapper <- function(nsim){
result <- IntTestAll(nx1=50,nmc=250)
return(result)
}

nSim <- 1000

# Running the function TreeTest nSim times
BigListResult = sfLapply(1:nSim,wrapper)

# Save the result to a folder on the cluster
save(BigListResult,file = 'ita_new005.rda')

# Close all connections
sfStop()
