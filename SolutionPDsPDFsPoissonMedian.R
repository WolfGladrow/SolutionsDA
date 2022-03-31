print('file: SolutionPDsPDFsPoissonMedian.R')
# EXECISE & SOLUTION: median of the Poisson distribution
# The median of the Poisson distribution lies between 
# \ben \lambda - \ln 2 < {\rm median} < \lambda + \frac{1}{3} \een
# (Adell, 2005). Apply Monte-Carlo simulations to investigate how good these limits are 
# for $\lambda$ between 0.5 and 3 in steps of 0.1.
set.seed(1953); M = 10^4; n = 5
lambdaArr = seq(0.5,3,0.1); L = length(lambdaArr)
LB = lambdaArr-log(2)   # lower bound
UB = lambdaArr+1/3      # upper bound
medianEst = numeric(M); meanMedian = numeric(L)
for(k in 1:L){lambdak = lambdaArr[k];
for(m in 1:M){medianEst[m] = median(rpois(n,lambdak))};
meanMedian[k] = mean(medianEst)}
library(latex2exp)
# png('PoissonMedian220228.png',width=16,height=16,units='cm',res=300)
plot(lambdaArr,LB,type='l',lwd=3,col='magenta',xlab=expression(lambda),
     ylab='Median',las=1,cex=0.4,ylim=c(min(LB),max(UB)),lty=2,cex.lab=1.5)
lines(lambdaArr,UB,lwd=3,col='red',lty=2)
lines(lambdaArr,meanMedian,lwd=3,col='blue')
lines(lambdaArr,lambdaArr,lwd=3,col='black',lty=3)
legend('topleft',legend=c('upper limit','lower limit','MC estimate',TeX('$\\lambda$')),
       col=c('red','magenta','blue','black'),lty=c(2,2,1,3),lwd=c(3,3,3,3),cex=1.2)
# dev.off()