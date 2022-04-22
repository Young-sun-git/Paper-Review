
load('ita_new005.rda')
BL005 = BigListResult

load('ita_new025.rda')
BL025 = BigListResult

load('ita_new050.rda')
BL050 = BigListResult




powerfn = function(BigList) 
{
  Sarray = matrix(NA,1000,14)
  
  for(i in 1:1000){
    Sarray[i,1] = BigList[[i]]$tstat.m2.500
    Sarray[i,2] = BigList[[i]]$tstat.m3.500
    Sarray[i,3] = BigList[[i]]$tstat.m5.500
    Sarray[i,4] = BigList[[i]]$tstat.m7.500
    Sarray[i,5] = BigList[[i]]$tstat.m8.500
    Sarray[i,6] = BigList[[i]]$tstat.m10.500
    Sarray[i,7] = BigList[[i]]$tstat.m11.500
    Sarray[i,8] = BigList[[i]]$tstat.m13.500
    Sarray[i,9] = BigList[[i]]$tstat.m14.500
    Sarray[i,10] = BigList[[i]]$tstat.m15.500
    Sarray[i,11] = BigList[[i]]$tstat.m17.500
    Sarray[i,12] = BigList[[i]]$tstat.m18.500
    Sarray[i,13] = BigList[[i]]$tstat.m19.500
    Sarray[i,14] = BigList[[i]]$tstat.m20.500
  }


  dfs = c(9,9,9,20,20,6,6,9,6,9,9,6,9,6)
  
  cuts = matrix(qchisq(0.95,dfs),1000,14,byrow=TRUE)
  
  powers = apply( Sarray > cuts, 2, mean)
}

p005 = powerfn(BL005)
p025 = powerfn(BL025)
p050 = powerfn(BL050)

nRPpower = cbind(p050,p025,p005)

RPpower = read.table('../RPpower.txt', )
RPpower = as.matrix(RPpower)


source('../EffectSizes.R')

smat = matrix(sizes,14,3,byrow=FALSE)
smat = sqrt(smat)%*%diag(c(2,4,20))

dev.new(width=8,height=4)
matplot(log(t(smat+1)),t(RPpower),type='b',lty=1,lwd=2,cex=1.2,xlab='log(effect+1)',ylab='Rejection Percentage',cex.lab=1,cex.axis=1,col=1,pch=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n'))
matplot(log(t(smat+1)),t(nRPpower),type='b',lty=2,lwd=2,cex=1.2,add=TRUE,col=1,pch=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n'))
abline(h=0.05)

# No effects

AllNulls = matrix(0,14,3)

AllNulls[2*(1:7)-1,] = nRPpower[1:7,]
AllNulls[2*(1:7),] = RPpower[1:7,]

# Alternatives

AllAlts = matrix(0,14,3)

AllAlts[2*(1:7)-1,] = nRPpower[7+1:7,]
AllAlts[2*(1:7),] = RPpower[7+1:7,]

# Put them all together

AllP = matrix(0,28,3)
AllP[4*(1:7)-3,] = nRPpower[1:7,]
AllP[4*(1:7)-2,] = nRPpower[7+(1:7),]
AllP[4*(1:7)-1,] = RPpower[1:7,]
AllP[4*(1:7),] = RPpower[7+(1:7),]



