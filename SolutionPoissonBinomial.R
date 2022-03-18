print('file: SolutionPoissonBinomial.R')
# EXERCISE & SOLUTION: compare binomial & Poisson distribution
n = 10; k = seq(0,n); p = 0.25; pk = dbinom(x=k,size=n,prob=p)
lambda1 = sum(k*pk)
pkp = dpois(k,lambda=lambda1)
# png('BinomPoisson160817.png',width=16,height=12,units='cm',res=300)
plot(k,pk,type='p',lwd=3,col='blue',xlab='k',ylim=c(0,max(pkp)*1.03),
     ylab='Probabilities',las=1,cex=0.5)
points(k,pkp,col='red',lwd=3,cex=0.5,pch=24)
# dev.off()