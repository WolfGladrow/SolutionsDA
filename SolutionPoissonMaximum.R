print('file: SolutionPoissonMaximum.R')
# Exercise & Solution: location of maximum of the Poisson PD
lambda = seq(0.01,2,0.01)
p0 = dpois(0,lambda)
p1 = dpois(1,lambda)
# png('Poissonp0p1x220328.png',width=16,height=16,units='cm',res=300)
plot(lambda,p0,type='l',lwd=4,col='black',xlab=TeX('$\\lambda$'),
ylab='Probability',las=1,cex=0.6,cex.lab=1.5,ylim=c(0,1))
lines(lambda,p1,col='magenta',lwd=4,lty=4)
abline(v=1,col='blue',lty=2)
# dev.off()