
n = 1000
cut = 0.5 + qnorm(0.05)/sqrt(12*n)

power01 = matrix(NA,6,3)
power10 = matrix(NA,6,3)

for(i in 1:3){
  for(j in 1:6){
    load(paste('Sim_Results_vimp01b',j,'d',i,'.rda',sep=''))
    BigList01 = BigListResult
    
    load(paste('Sim_Results_vimp10b',j,'d',i,'.rda',sep=''))
    BigList10 = BigListResult
    
    mpval01 = rep(NA,n)
    mpval10 = rep(NA,n)
    for(k in 1:1000){ 
      mpval01[k] = BigList01[[k]]$mean.pval
      mpval10[k] = BigList10[[k]]$mean.pval 
    }
    power01[j,i] = mean( mpval01 < cut )
    power10[j,i] = mean( mpval10 < cut )
  }
}

betas = c(0,0.1,0.25,0.5,1,2)
matplot(betas,power01,type='b',lwd=3,lty=1,pch=1:3,cex=2,cex.axis=2,cex.lab=1.5,xlab=expression('beta'),ylim = c(0,1))
matplot(betas,power10,type='b',lwd=3,lty=2,pch=1:3,cex=2,add=TRUE)
abline(h=0.05)
legend('bottomright',legend=c('d=5','d=10','d=20'),col=1:3,pch=1:3,lty=2,cex=2,lwd=3)