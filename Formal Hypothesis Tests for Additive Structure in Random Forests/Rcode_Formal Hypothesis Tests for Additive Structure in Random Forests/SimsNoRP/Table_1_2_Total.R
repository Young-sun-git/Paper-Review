IntTestAll <- function(nx1=50,nmc=250,minsplit=3,maxcompete=0,maxsurrogate=0,usesurrogate=0) {
	
tstat.LM.add.250 <- 0
tstat.LM.add.500 <- 0
tstat.LM.add.1000 <- 0
tstat.LM.int.250 <- 0
tstat.LM.int.500 <- 0
tstat.LM.int.1000 <- 0

tstat.mod1 <- 0
tstat.mod2 <- 0
tstat.mod3 <- 0
tstat.mod4 <- 0
tstat.mod5 <- 0
tstat.mod8 <- 0
tstat.mod10 <- 0
tstat.mod11 <- 0
tstat.mod13 <- 0

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

# LM:  250
x1.LM.add.250 <- runif(250)
x2.LM.add.250 <- runif(250)
x1.LM.int.250 <- runif(250)
x2.LM.int.250 <- runif(250)
eps.add <- rnorm(250,0,sd=0.05)
eps.int <- rnorm(250,0,sd=0.05)
y.add.250 <- x1.LM.add.250 + x2.LM.add.250 + eps.add
y.int.250 <- x1.LM.int.250 + x2.LM.int.250 + x1.LM.int.250*x2.LM.int.250 + eps.int
df.add.250 <- data.frame("x1"=x1.LM.add.250,"x2"=x2.LM.add.250,"y"=y.add.250)
df.int.250 <- data.frame("x1"=x1.LM.int.250,"x2"=x2.LM.int.250,"y"=y.int.250)

# LM:  500
x1.LM.add.500 <- runif(500)
x2.LM.add.500 <- runif(500)
x1.LM.int.500 <- runif(500)
x2.LM.int.500 <- runif(500)
eps.add <- rnorm(500,0,sd=0.05)
eps.int <- rnorm(500,0,sd=0.05)
y.add.500 <- x1.LM.add.500 + x2.LM.add.500 + eps.add
y.int.500 <- x1.LM.int.500 + x2.LM.int.500 + x1.LM.int.500*x2.LM.int.500 + eps.int
df.add.500 <- data.frame("x1"=x1.LM.add.500,"x2"=x2.LM.add.500,"y"=y.add.500)
df.int.500 <- data.frame("x1"=x1.LM.int.500,"x2"=x2.LM.int.500,"y"=y.int.500)

# LM:  1000
x1.LM.add.1000 <- runif(1000)
x2.LM.add.1000 <- runif(1000)
x1.LM.int.1000 <- runif(1000)
x2.LM.int.1000 <- runif(1000)
eps.add <- rnorm(1000,0,sd=0.05)
eps.int <- rnorm(1000,0,sd=0.05)
y.add.1000 <- x1.LM.add.1000 + x2.LM.add.1000 + eps.add
y.int.1000 <- x1.LM.int.1000 + x2.LM.int.1000 + x1.LM.int.1000*x2.LM.int.1000 + eps.int
df.add.1000 <- data.frame("x1"=x1.LM.add.1000,"x2"=x2.LM.add.1000,"y"=y.add.1000)
df.int.1000 <- data.frame("x1"=x1.LM.int.1000,"x2"=x2.LM.int.1000,"y"=y.int.1000)

# Model 1:  y = x1 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500 + eps.500
df.mod1 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 2:  y = exp(x1) 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + eps.500
df.mod2 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 3:  y = exp(x1) + sin(pi x2) 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + sin(pi*x2.500) + eps.500
df.mod3 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 4:  y = x1 + x2 + x3 
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500 + x2.500 + x3.500 + eps.500
df.mod4 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 5:  y = exp(x1) + exp(x2) + exp(x3) 
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500) + exp(x2.500) + exp(x3.500) + eps.500
df.mod5 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 8:  y = x1*x2 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500*x2.500 + eps.500
df.mod8 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 10:  y = exp(5*(x1+x2))/(1+exp(5*(x1+x2))) - 1
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(5*(x1.500+x2.500))/(1+exp(5*(x1.500+x2.500))) - 1 + eps.500
df.mod10 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 11:  y = 0.5*(1+sin(2*pi*(x1+x2))) 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- 0.5*(1+sin(2*pi*(x1.500+x2.500))) + eps.500
df.mod11 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)

# Model 13:  y = (64*(x1*x2)^3)*(1-x1*x2)^3 
x1.500 <- runif(500)
x2.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- (64*(x1.500*x2.500)^3)*(1-x1.500*x2.500)^3 + eps.500
df.mod13 <- data.frame("x1"=x1.500,"x2"=x2.500,"y"=y.500)


######################################################################
#########################  Preallocating  ############################
######################################################################
cond.exp.LM.add.250 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.LM.add.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.LM.add.1000 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.LM.int.250 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.LM.int.500 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.LM.int.1000 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)

cond.exp.mod1 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod2 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod3 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod4 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod5 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod8 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod10 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod11 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)
cond.exp.mod13 <- matrix(rep(0,nx1*dim(test.grid.2D)[1]),nrow=nx1)

full.pred.LM.add.250 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.LM.add.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.LM.add.1000 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.LM.int.250 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.LM.int.500 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.LM.int.1000 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)

full.pred.mod1 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod2 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod3 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod4 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod5 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod8 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod10 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod11 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)
full.pred.mod13 <- matrix(rep(100,dim(test.grid.2D)[1]),nrow=1)

######################################################################
#################  Building Trees and Predicting  ####################
######################################################################

for (i in 1:nx1) {

	ind.x1.250 <- sample(1:250,size=1,replace=FALSE)
	ind.x1.500 <- sample(1:500,size=1,replace=FALSE)
	ind.x1.1000 <- sample(1:1000,size=1,replace=FALSE)

	pred.LM.add.250 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.LM.add.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.LM.add.1000 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.LM.int.250 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.LM.int.500 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.LM.int.1000 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)

	pred.mod1 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.mod2 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.mod3 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.mod4 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod5 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod8 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.mod10 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	pred.mod11 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc
	pred.mod13 <- matrix(rep(0,nmc*dim(test.grid.2D)[1]),nrow=nmc)
	
	for (j in 1:nmc) {

		ind.250 <- c(ind.x1.250,sample((1:250)[-ind.x1.250],29,replace=FALSE))
		ind.500 <- c(ind.x1.500,sample((1:500)[-ind.x1.500],49,replace=FALSE))
		ind.1000 <- c(ind.x1.1000,sample((1:1000)[-ind.x1.1000],74,replace=FALSE))

		ss.LM.add.250 <- df.add.250[ind.250,]
		ss.LM.add.500 <- df.add.500[ind.500,]
		ss.LM.add.1000 <- df.add.1000[ind.1000,]
		ss.LM.int.250 <- df.int.250[ind.250,]
		ss.LM.int.500 <- df.int.500[ind.500,]
		ss.LM.int.1000 <- df.int.1000[ind.1000,]
		
		ss.mod1 <- df.mod1[ind.500,]
		ss.mod2 <- df.mod2[ind.500,]
		ss.mod3 <- df.mod3[ind.500,]
		ss.mod4 <- df.mod4[ind.500,]
		ss.mod5 <- df.mod5[ind.500,]
		ss.mod8 <- df.mod8[ind.500,]
		ss.mod10 <- df.mod10[ind.500,]
		ss.mod11 <- df.mod11[ind.500,]
		ss.mod13 <- df.mod13[ind.500,]
		
		tree.LM.add.250 <- rpart(y~.,data=ss.LM.add.250,control=control.sim)
		tree.LM.add.500 <- rpart(y~.,data=ss.LM.add.500,control=control.sim)
		tree.LM.add.1000 <- rpart(y~.,data=ss.LM.add.1000,control=control.sim)
		tree.LM.int.250 <- rpart(y~.,data=ss.LM.int.250,control=control.sim)
		tree.LM.int.500 <- rpart(y~.,data=ss.LM.int.500,control=control.sim)
		tree.LM.int.1000 <- rpart(y~.,data=ss.LM.int.1000,control=control.sim)
		
		tree.mod1 <- rpart(y~.,data=ss.mod1,control=control.sim)
		tree.mod2 <- rpart(y~.,data=ss.mod2,control=control.sim)
		tree.mod3 <- rpart(y~.,data=ss.mod3,control=control.sim)
		tree.mod4 <- rpart(y~.,data=ss.mod4,control=control.sim)
		tree.mod5 <- rpart(y~.,data=ss.mod5,control=control.sim)
		tree.mod8 <- rpart(y~.,data=ss.mod8,control=control.sim)
		tree.mod10 <- rpart(y~.,data=ss.mod10,control=control.sim)
		tree.mod11 <- rpart(y~.,data=ss.mod11,control=control.sim)
		tree.mod13 <- rpart(y~.,data=ss.mod13,control=control.sim)
		
		pred.LM.add.250[j,] <- predict(tree.LM.add.250,test.grid.2D)
		pred.LM.add.500[j,] <- predict(tree.LM.add.500,test.grid.2D)
		pred.LM.add.1000[j,] <- predict(tree.LM.add.1000,test.grid.2D)
		pred.LM.int.250[j,] <- predict(tree.LM.int.250,test.grid.2D)
		pred.LM.int.500[j,] <- predict(tree.LM.int.500,test.grid.2D)
		pred.LM.int.1000[j,] <- predict(tree.LM.int.1000,test.grid.2D)
		
		pred.mod1[j,] <- predict(tree.mod1,test.grid.2D)
		pred.mod2[j,] <- predict(tree.mod2,test.grid.2D)
		pred.mod3[j,] <- predict(tree.mod3,test.grid.2D)
		pred.mod4[j,] <- predict(tree.mod4,test.grid.3D)
		pred.mod5[j,] <- predict(tree.mod5,test.grid.3D)
		pred.mod8[j,] <- predict(tree.mod8,test.grid.2D)
		pred.mod10[j,] <- predict(tree.mod10,test.grid.2D)
		pred.mod11[j,] <- predict(tree.mod11,test.grid.2D)
		pred.mod13[j,] <- predict(tree.mod13,test.grid.2D)
		
		cat("nx1:  ",i,"        nmc: ",j,"\n")

	}

	cond.exp.LM.add.250[i,] <- apply(pred.LM.add.250,2,mean)
	cond.exp.LM.add.500[i,] <- apply(pred.LM.add.500,2,mean)
	cond.exp.LM.add.1000[i,] <- apply(pred.LM.add.1000,2,mean)
	cond.exp.LM.int.250[i,] <- apply(pred.LM.int.250,2,mean)
	cond.exp.LM.int.500[i,] <- apply(pred.LM.int.500,2,mean)
	cond.exp.LM.int.1000[i,] <- apply(pred.LM.int.1000,2,mean)
	
	cond.exp.mod1[i,] <- apply(pred.mod1,2,mean)
	cond.exp.mod2[i,] <- apply(pred.mod2,2,mean)
	cond.exp.mod3[i,] <- apply(pred.mod3,2,mean)
	cond.exp.mod4[i,] <- apply(pred.mod4,2,mean)
	cond.exp.mod5[i,] <- apply(pred.mod5,2,mean)
	cond.exp.mod8[i,] <- apply(pred.mod8,2,mean)
	cond.exp.mod10[i,] <- apply(pred.mod10,2,mean)
	cond.exp.mod11[i,] <- apply(pred.mod11,2,mean)
	cond.exp.mod13[i,] <- apply(pred.mod13,2,mean)
	
	full.pred.LM.add.250 <- rbind(full.pred.LM.add.250,pred.LM.add.250)
	full.pred.LM.add.500 <- rbind(full.pred.LM.add.500,pred.LM.add.500)
	full.pred.LM.add.1000 <- rbind(full.pred.LM.add.1000,pred.LM.add.1000)
	full.pred.LM.int.250 <- rbind(full.pred.LM.int.250,pred.LM.int.250)
	full.pred.LM.int.500 <- rbind(full.pred.LM.int.500,pred.LM.int.500)
	full.pred.LM.int.1000 <- rbind(full.pred.LM.int.1000,pred.LM.int.1000)
	
	full.pred.mod1 <- rbind(full.pred.mod1,pred.mod1)
	full.pred.mod2 <- rbind(full.pred.mod2,pred.mod2)
	full.pred.mod3 <- rbind(full.pred.mod3,pred.mod3)
	full.pred.mod4 <- rbind(full.pred.mod4,pred.mod4)
	full.pred.mod5 <- rbind(full.pred.mod5,pred.mod5)
	full.pred.mod8 <- rbind(full.pred.mod8,pred.mod8)
	full.pred.mod10 <- rbind(full.pred.mod10,pred.mod10)
	full.pred.mod11 <- rbind(full.pred.mod11,pred.mod11)
	full.pred.mod13 <- rbind(full.pred.mod13,pred.mod13)
}


full.pred.LM.add.250 <- full.pred.LM.add.250[-1,]
full.pred.LM.add.500 <- full.pred.LM.add.500[-1,]
full.pred.LM.add.1000 <- full.pred.LM.add.1000[-1,]
full.pred.LM.int.250 <- full.pred.LM.int.250[-1,]
full.pred.LM.int.500 <- full.pred.LM.int.500[-1,]
full.pred.LM.int.1000 <- full.pred.LM.int.1000[-1,]

full.pred.mod1 <- full.pred.mod1[-1,]
full.pred.mod2 <- full.pred.mod2[-1,]
full.pred.mod3 <- full.pred.mod3[-1,]
full.pred.mod4 <- full.pred.mod4[-1,]
full.pred.mod5 <- full.pred.mod5[-1,]
full.pred.mod8 <- full.pred.mod8[-1,]
full.pred.mod10 <- full.pred.mod10[-1,]
full.pred.mod11 <- full.pred.mod11[-1,]
full.pred.mod13 <- full.pred.mod13[-1,]

mean.LM.add.250 <- apply(full.pred.LM.add.250,2,mean)
mean.LM.add.500 <- apply(full.pred.LM.add.500,2,mean)
mean.LM.add.1000 <- apply(full.pred.LM.add.1000,2,mean)
mean.LM.int.250 <- apply(full.pred.LM.int.250,2,mean)
mean.LM.int.500 <- apply(full.pred.LM.int.500,2,mean)
mean.LM.int.1000 <- apply(full.pred.LM.int.1000,2,mean)

mean.mod1 <- apply(full.pred.mod1,2,mean)
mean.mod2 <- apply(full.pred.mod2,2,mean)
mean.mod3 <- apply(full.pred.mod3,2,mean)
mean.mod4 <- apply(full.pred.mod4,2,mean)
mean.mod5 <- apply(full.pred.mod5,2,mean)
mean.mod8 <- apply(full.pred.mod8,2,mean)
mean.mod10 <- apply(full.pred.mod10,2,mean)
mean.mod11 <- apply(full.pred.mod11,2,mean)
mean.mod13 <- apply(full.pred.mod13,2,mean)


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

DV.LM.add.250 <- D.2D %*% mean.LM.add.250
DV.LM.add.500 <- D.2D %*% mean.LM.add.500
DV.LM.add.1000 <- D.2D %*% mean.LM.add.1000
DV.LM.int.250 <- D.2D %*% mean.LM.int.250
DV.LM.int.500 <- D.2D %*% mean.LM.int.500
DV.LM.int.1000 <- D.2D %*% mean.LM.int.1000

DV.mod1 <- D.2D %*% mean.mod1
DV.mod2 <- D.2D %*% mean.mod2
DV.mod3 <- D.2D %*% mean.mod3
DV.mod4 <- D.3D %*% mean.mod4
DV.mod5 <- D.3D %*% mean.mod5
DV.mod8 <- D.2D %*% mean.mod8
DV.mod10 <- D.2D %*% mean.mod10
DV.mod11 <- D.2D %*% mean.mod11
DV.mod13 <- D.2D %*% mean.mod13


########################################################################
#################  Calculating the Test Statistics  ####################
########################################################################

m <- nx1*nmc
tstat.LM.add.250 <- t(DV.LM.add.250) %*% ginv(D.2D %*% ((m/250)*((30^2)/m)*cov(cond.exp.LM.add.250) + (1/m)*cov(full.pred.LM.add.250)) %*% t(D.2D)) %*% DV.LM.add.250
tstat.LM.add.500 <- t(DV.LM.add.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.LM.add.500) + (1/m)*cov(full.pred.LM.add.500)) %*% t(D.2D)) %*% DV.LM.add.500
tstat.LM.add.1000 <- t(DV.LM.add.1000) %*% ginv(D.2D %*% ((m/1000)*((75^2)/m)*cov(cond.exp.LM.add.1000) + (1/m)*cov(full.pred.LM.add.1000)) %*% t(D.2D)) %*% DV.LM.add.1000
tstat.LM.int.250 <- t(DV.LM.int.250) %*% ginv(D.2D %*% ((m/250)*((30^2)/m)*cov(cond.exp.LM.int.250) + (1/m)*cov(full.pred.LM.int.250)) %*% t(D.2D)) %*% DV.LM.int.250
tstat.LM.int.500 <- t(DV.LM.int.500) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.LM.int.500) + (1/m)*cov(full.pred.LM.int.500)) %*% t(D.2D)) %*% DV.LM.int.500
tstat.LM.int.1000 <- t(DV.LM.int.1000) %*% ginv(D.2D %*% ((m/1000)*((75^2)/m)*cov(cond.exp.LM.int.1000) + (1/m)*cov(full.pred.LM.int.1000)) %*% t(D.2D)) %*% DV.LM.int.1000

tstat.mod1 <- t(DV.mod1) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod1) + (1/m)*cov(full.pred.mod1)) %*% t(D.2D)) %*% DV.mod1
tstat.mod2 <- t(DV.mod2) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod2) + (1/m)*cov(full.pred.mod2)) %*% t(D.2D)) %*% DV.mod2
tstat.mod3 <- t(DV.mod3) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod3) + (1/m)*cov(full.pred.mod3)) %*% t(D.2D)) %*% DV.mod3
tstat.mod4 <- t(DV.mod4) %*% ginv(D.3D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod4) + (1/m)*cov(full.pred.mod4)) %*% t(D.3D)) %*% DV.mod4
tstat.mod5 <- t(DV.mod5) %*% ginv(D.3D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod5) + (1/m)*cov(full.pred.mod5)) %*% t(D.3D)) %*% DV.mod5
tstat.mod8 <- t(DV.mod8) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod8) + (1/m)*cov(full.pred.mod8)) %*% t(D.2D)) %*% DV.mod8
tstat.mod10 <- t(DV.mod10) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod10) + (1/m)*cov(full.pred.mod10)) %*% t(D.2D)) %*% DV.mod10
tstat.mod11 <- t(DV.mod11) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod11) + (1/m)*cov(full.pred.mod11)) %*% t(D.2D)) %*% DV.mod11
tstat.mod13 <- t(DV.mod13) %*% ginv(D.2D %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod13) + (1/m)*cov(full.pred.mod13)) %*% t(D.2D)) %*% DV.mod13

return(list("tstat.LM.add.250"=tstat.LM.add.250,"tstat.LM.add.500"=tstat.LM.add.500,"tstat.LM.add.1000"=tstat.LM.add.1000,"tstat.LM.int.250"=tstat.LM.int.250,"tstat.LM.int.500"=tstat.LM.int.500,"tstat.LM.int.1000"=tstat.LM.int.1000,"tstat.mod1"=tstat.mod1,"tstat.mod2"=tstat.mod2,"tstat.mod3"=tstat.mod3,"tstat.mod4"=tstat.mod4,"tstat.mod5"=tstat.mod5,"tstat.mod8"=tstat.mod8,"tstat.mod10"=tstat.mod10,"tstat.mod11"=tstat.mod11,"tstat.mod13"=tstat.mod13))

}
