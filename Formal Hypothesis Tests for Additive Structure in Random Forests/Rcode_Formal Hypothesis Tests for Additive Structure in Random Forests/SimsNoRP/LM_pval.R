IntTestPval <- function(nsim) {

pval.add.250 <- rep(0,nsim)	
pval.add.500 <- rep(0,nsim)	
pval.add.1000 <- rep(0,nsim)	
pval.int.250 <- rep(0,nsim)	
pval.int.500 <- rep(0,nsim)	
pval.int.1000 <- rep(0,nsim)	

for (i in 1:nsim) {
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
pval.add.250[i] <- summary(lm(y~x1*x2,data=df.add.250))$coefficients[4,4]
pval.int.250[i] <- summary(lm(y~x1*x2,data=df.int.250))$coefficients[4,4]

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
pval.add.500[i] <- summary(lm(y~x1*x2,data=df.add.500))$coefficients[4,4]
pval.int.500[i] <- summary(lm(y~x1*x2,data=df.int.500))$coefficients[4,4]

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
pval.add.1000[i] <- summary(lm(y~x1*x2,data=df.add.1000))$coefficients[4,4]
pval.int.1000[i] <- summary(lm(y~x1*x2,data=df.int.1000))$coefficients[4,4]

cat("Iteration  ",i,"\n")
}
return(list("pval.add.250"=pval.add.250,"pval.add.500"=pval.add.500,"pval.add.1000"=pval.add.1000,"pval.int.250"=pval.int.250,"pval.int.500"=pval.int.500,"pval.int.1000"=pval.int.1000))
}

pval <- IntTestPval(1000)
(alpha.add.250 <- length(which(pval$pval.add.250<0.05))/1000)
(alpha.add.500 <- length(which(pval$pval.add.500<0.05))/1000)
(alpha.add.1000 <- length(which(pval$pval.add.1000<0.05))/1000)
(alpha.int.250 <- length(which(pval$pval.int.250<0.05))/1000)
(alpha.int.500 <- length(which(pval$pval.int.500<0.05))/1000)
(alpha.int.500 <- length(which(pval$pval.int.1000<0.05))/1000)