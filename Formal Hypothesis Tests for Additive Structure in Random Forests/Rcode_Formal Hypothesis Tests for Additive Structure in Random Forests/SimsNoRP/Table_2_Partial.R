IntTestAll.part <- function(nx1=50,nmc=250,minsplit=3,maxcompete=0,maxsurrogate=0,usesurrogate=0) {
	
tstat.mod6 <- 0
tstat.mod7 <- 0
tstat.mod9 <- 0
tstat.mod12 <- 0
tstat.mod14 <- 0

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

# Model 6:  y = x1*x3 + x2*x3
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500*x3.500 + x2.500*x3.500 + eps.500
df.mod6 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 7:  y = exp(x1*x3) + exp(x2*x3)
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- exp(x1.500*x3.500) + exp(x2.500*x3.500) + eps.500
df.mod7 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 9:  y = x1*x2*x3 
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- x1.500*x2.500*x3.500 + eps.500
df.mod9 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 12:  y = 0.5*(1+sin(2*pi*(x1+x2+x3)))
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- 0.5*(1+sin(2*pi*(x1.500+x2.500+x3.500))) + eps.500
df.mod12 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)

# Model 14:  y = (64*(x1*x2*x3)^3)*(1-x1*x2*x3)^3 
x1.500 <- runif(500)
x2.500 <- runif(500)
x3.500 <- runif(500)
eps.500 <- rnorm(500,0,sd=0.05)
y.500 <- (64*(x1.500*x2.500*x3.500)^3)*(1-x1.500*x2.500*x3.500)^3 + eps.500
df.mod14 <- data.frame("x1"=x1.500,"x2"=x2.500,"x3"=x3.500,"y"=y.500)


######################################################################
#########################  Preallocating  ############################
######################################################################

cond.exp.mod6 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod7 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod9 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod12 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)
cond.exp.mod14 <- matrix(rep(0,nx1*dim(test.grid.3D)[1]),nrow=nx1)

full.pred.mod6 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod7 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod9 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod12 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)
full.pred.mod14 <- matrix(rep(100,dim(test.grid.3D)[1]),nrow=1)

######################################################################
#################  Building Trees and Predicting  ####################
######################################################################

for (i in 1:nx1) {

	ind.x1.500 <- sample(1:500,size=1,replace=FALSE)
	
	pred.mod6 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod7 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod9 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod12 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)
	pred.mod14 <- matrix(rep(0,nmc*dim(test.grid.3D)[1]),nrow=nmc)

	for (j in 1:nmc) {

		ind.500 <- c(ind.x1.500,sample((1:500)[-ind.x1.500],49,replace=FALSE))

		ss.mod6 <- df.mod6[ind.500,]
		ss.mod7 <- df.mod7[ind.500,]
		ss.mod9 <- df.mod9[ind.500,]
		ss.mod12 <- df.mod12[ind.500,]
		ss.mod14 <- df.mod14[ind.500,]
		
		tree.mod6 <- rpart(y~.,data=ss.mod6,control=control.sim)
		tree.mod7 <- rpart(y~.,data=ss.mod7,control=control.sim)
		tree.mod9 <- rpart(y~.,data=ss.mod9,control=control.sim)
		tree.mod12 <- rpart(y~.,data=ss.mod12,control=control.sim)
		tree.mod14 <- rpart(y~.,data=ss.mod14,control=control.sim)
		
		pred.mod6[j,] <- predict(tree.mod6,test.grid.3D)
		pred.mod7[j,] <- predict(tree.mod7,test.grid.3D)
		pred.mod9[j,] <- predict(tree.mod9,test.grid.3D)
		pred.mod12[j,] <- predict(tree.mod12,test.grid.3D)
		pred.mod14[j,] <- predict(tree.mod14,test.grid.3D)
		
		cat("nx1:  ",i,"        nmc: ",j,"\n")

	}
	
	cond.exp.mod6[i,] <- apply(pred.mod6,2,mean)
	cond.exp.mod7[i,] <- apply(pred.mod7,2,mean)
	cond.exp.mod9[i,] <- apply(pred.mod9,2,mean)
	cond.exp.mod12[i,] <- apply(pred.mod12,2,mean)
	cond.exp.mod14[i,] <- apply(pred.mod14,2,mean)
	
	full.pred.mod6 <- rbind(full.pred.mod6,pred.mod6)
	full.pred.mod7 <- rbind(full.pred.mod7,pred.mod7)
	full.pred.mod9 <- rbind(full.pred.mod9,pred.mod9)
	full.pred.mod12 <- rbind(full.pred.mod12,pred.mod12)
	full.pred.mod14 <- rbind(full.pred.mod14,pred.mod14)
}

full.pred.mod6 <- full.pred.mod6[-1,]
full.pred.mod7 <- full.pred.mod7[-1,]
full.pred.mod9 <- full.pred.mod9[-1,]
full.pred.mod12 <- full.pred.mod12[-1,]
full.pred.mod14 <- full.pred.mod14[-1,]

mean.mod6 <- apply(full.pred.mod6,2,mean)
mean.mod7 <- apply(full.pred.mod7,2,mean)
mean.mod9 <- apply(full.pred.mod9,2,mean)
mean.mod12 <- apply(full.pred.mod12,2,mean)
mean.mod14 <- apply(full.pred.mod14,2,mean)


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
D.part <- diag(27) - Di.k - D.jk + D..k

DV.mod6 <- D.part %*% mean.mod6
DV.mod7 <- D.part %*% mean.mod7
DV.mod9 <- D.part %*% mean.mod9
DV.mod12 <- D.part %*% mean.mod12
DV.mod14 <- D.part %*% mean.mod14


########################################################################
#################  Calculating the Test Statistics  ####################
########################################################################

m <- nx1*nmc

tstat.mod6 <- t(DV.mod6) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod6) + (1/m)*cov(full.pred.mod6)) %*% t(D.part)) %*% DV.mod6
tstat.mod7 <- t(DV.mod7) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod7) + (1/m)*cov(full.pred.mod7)) %*% t(D.part)) %*% DV.mod7
tstat.mod9 <- t(DV.mod9) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod9) + (1/m)*cov(full.pred.mod9)) %*% t(D.part)) %*% DV.mod9
tstat.mod12 <- t(DV.mod12) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod12) + (1/m)*cov(full.pred.mod12)) %*% t(D.part)) %*% DV.mod12
tstat.mod14 <- t(DV.mod14) %*% ginv(D.part %*% ((m/500)*((50^2)/m)*cov(cond.exp.mod14) + (1/m)*cov(full.pred.mod14)) %*% t(D.part)) %*% DV.mod14

return(list("tstat.mod6"=tstat.mod6,"tstat.mod7"=tstat.mod7,"tstat.mod9"=tstat.mod9,"tstat.mod12"=tstat.mod12,"tstat.mod14"=tstat.mod14))

}
