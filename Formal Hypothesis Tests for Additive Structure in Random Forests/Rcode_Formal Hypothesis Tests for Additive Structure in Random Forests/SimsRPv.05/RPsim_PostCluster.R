# load the data:
load("/Users/lucasmentch/Desktop/RP Simulations/Results/Sim_Results_m14.rda")

# Extract the elements:
pvals <- rep(0,1000)
for (i in 1:1000) {
	pvals[i] <- BigListResult[[i]]$mean.pval
}

hist(pvals)
max(pvals)

# Estimating the cutoff
p <- rep(0,100000)
for (i in 1:100000) {
	p[i] <- mean(runif(1000))
	cat("interation:  ",i,"\n")
}

(cutoff <- quantile(p,0.05)) # 0.484943

mean(pvals<0.484943)   # 0.059