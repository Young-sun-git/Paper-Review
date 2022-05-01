
n = 1000
cut = 0.5 + qnorm(0.05)/sqrt(12*n)

Tpower01 = matrix(NA,6,3)
Tpower10 = matrix(NA,6,3)

Ppower01 = matrix(NA,6,3)
Ppower10 = matrix(NA,6,3)

for(i in 1:3){
  for(j in 1:6){
    load(paste('Sim_Results_intv01b',j,'d',i,'.rda',sep=''))
    Blist01 = BigListResult
    
    load(paste('Sim_Results_intv10b',j,'d',i,'.rda',sep=''))
    Blist10 = BigListResult
    
    
    Tmpval01 = rep(NA,n)
    Tmpval10 = rep(NA,n)
    Pmpval01 = rep(NA,n)
    Pmpval10 = rep(NA,n)
    for(k in 1:1000){ 
       Tmpval01[k] = Blist01[[k]]$mean.pval 
       Tmpval10[k] = Blist10[[k]]$mean.pval 
       Pmpval01[k] = Blist01[[k]]$mean.pval.part 
       Pmpval10[k] = Blist10[[k]]$mean.pval.part 
    }
    Tpower01[j,i] = mean( Tmpval01 < cut )
    Tpower10[j,i] = mean( Tmpval10 < cut )
    Ppower01[j,i] = mean( Pmpval01 < cut )
    Ppower10[j,i] = mean( Pmpval10 < cut )
  }
}

betas = c(0,0.1,0.25,0.5,1,2)

matplot(betas,Tpower01,type='b',lwd=3,pch=1:3,cex=2,lty=1,cex.axis=2,cex.lab=1.5,xlab=expression(beta),ylab='Power',ylim =c(0,1))
matplot(betas,Tpower10,type='b',lwd=3,pch=1:3,cex=2,lty=2,add=TRUE)
abline(h = 0.05)
legend('bottomright',legend=c('d=5','d=10','d=20'),col=1:3,pch=1:3,lty=1,cex=2,lwd=3)

dev.new()
matplot(betas,Ppower01,type='b',lwd=3,pch=1:3,cex=2,lty=1,cex.axis=2,cex.lab=1.5,xlab=expression('beta'),ylab='Power',ylim =c(0,1))
matplot(betas,Ppower10,type='b',lwd=3,pch=1:3,cex=2,lty=2,add=TRUE)
abline(h = 0.05)
legend('bottomright',legend=c('d=5','d=10','d=20'),col=1:3,pch=1:3,lty=1,cex=2,lwd=3)
