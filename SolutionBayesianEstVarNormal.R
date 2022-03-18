print('file: SolutionBayesianEstVarNormal.R')
# EXERCISE & SOLUTION: Bayesian estimation of variance: normal pop., mean known
set.seed(1953)
n = 10; print(c(n,'sample size'))
sigma0sq = 4; print(c(sigma0sq,'true variance sigma_0^2 (known)'))
sigma0 = sqrt(sigma0sq)
mu = 3; print(c(mu,'mu true mean'))
muEst = 3.268; print(c(round(muEst,4),'sample mean'))
ssq = 2.837; print(c(round(ssq,4),'sample variance'))
tflag = 0 #1e6 # larger sample size
if (tflag > 0) {n = tflag; # muEst = 2.9974; ssq = 3.8988;
print(c(n,'modified sample size'))
y = rnorm(n,mu,sigma0) # n random numbers from normal PDF 
muEst = mean(y); print(c(round(muEst,4),'sample mean'))
ssq = var(y); print(c(round(ssq,4),'sample variance'))
dx1 = 0.001; xp1 = seq(1,5,dx1); yp1 = dnorm(xp1,mu,sigma0)
SE = sigma0/sqrt(n); print(c(round(SE,4),'standard error of mean'))
plot(density(y,from=1,to=5),type='l',main='',col='blue',las=1,
     xlab='x',lwd=3,ylim=c(0,max(yp1)))
lines(xp1,yp1,col='black',lwd=3,lty=2)
}
nu = n-1 # degrees of freedom
SE1 = sigma0/sqrt(n); print(c(round(SE1,4),'standard error of mean1'))
SE2 = sqrt(ssq/n); print(c(round(SE2,4),'standard error of mean2'))
alpha = n/2
beta = nu*ssq/2
library('invgamma')
dx = 0.001; x = seq(dx,11,dx)
IG = dinvgamma(x,shape=alpha,rate=beta)
mean1 = beta/(alpha-1); print(c(round(mean1,3),'mean1'))
mode1 = beta/(alpha+1); print(c(round(mode1,3),'mode1'))
# median: no analytical expression -> numerical via CDF
Lcdf = length(x)
CDF = numeric(Lcdf)
CDF[1] = IG[1]*dx
for(i in 2:Lcdf) CDF[i] = CDF[i-1] + IG[i]*dx
(median1 = x[which.min((CDF-0.5)^2)])
print(c(round(median1,3),'median1'))
print(' ---------------------------------------------------')
print('95% interval')
ahalf = 0.05/2
I95L = x[which.min((CDF-ahalf)^2)]; print(c(round(I95L,4),'I95L'))
I95U = x[which.min((CDF-(1-ahalf))^2)]; print(c(round(I95U,4),'I95U'))
xI95 = c(I95L,I95U); yI95 = c(0,0)
library(latex2exp)
# png('PosteriorNormalMuKnown210618.png',width=16,height=12,units='cm',res=300)
plot(x,IG,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,
     cex=0.4,xlim=c(0,10))
title(ylab=TeX('Inverse gamma PDF $(\\sigma^2 | y, \\mu_0)$'),line=2.3,cex.lab=1.5)
title(xlab=TeX('$\\sigma^2$'),cex.lab=1.5)
abline(v=sigma0sq,col='black')
abline(v=mean1,col='blue',lty=2)
abline(v=mode1,col='red',lty=3)
abline(v=median1,col='magenta',lty=4)
lines(xI95,yI95,col='magenta',lwd=3)
text(mean1-0.2,0.015,'95 % interval',col='magenta',pos=4,cex=1.5)
# dev.off()