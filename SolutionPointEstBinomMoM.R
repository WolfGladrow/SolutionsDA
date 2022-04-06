print('file: SolutionPointEstBinomMoM.R')
# EXERCISE & SOLUTION: binomial estimators: method of moments
set.seed(1953) # set seed for random number generators
k = 10    # number of trials
p = 0.4   # probability for success in single trials
M = 1e3   # number of Monte Carlo runs
n = 12    # sample size
kEst = numeric(M)
xmEst = numeric(M)
pEst = numeric(M)
for(i in 1:M) {
  x = rbinom(n,k,p); xmean = mean(x); m1 = var(x)*(n-1)/n;
  kEst[i] = xmean^2/(xmean-m1); xmEst[i] = xmean
}
kEstmin=min(kEst)
kEstmax=max(kEst)
cL = 0
cU = 0
for(i in 1:M) {
  if(kEst[i] < 0) cL = cL+1
  if(kEst[i] > 2*k) cU = cU+1
  if ((kEst[i] < 0) || (kEst[i] > 2*k)) kEst[i] = NA
  pEst[i] = xmEst[i]/kEst[i]
}
kEstm = mean(kEst,na.rm=TRUE); print(c(round(kEstm,2),' kEstm'))
ukEstm = sd(kEst,na.rm=TRUE); print(c(round(ukEstm,2),' ukEstm'))
pEstm = mean(pEst,na.rm=TRUE); print(c(round(pEstm,4),' pEstm'))
upEstm = sd(pEst,na.rm=TRUE); print(c(round(upEstm,3),' upEstm'))
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('BinomEstkMoM181228.png',width=16,height=16,units='cm',res=300)
  hist(kEst,col='blue',breaks=33,las=1,main='',xlab=expression(hat(k)),cex.lab=1.5)
  abline(v=k,col='black')
  abline(v=kEstm,col='blue',lty=4)
  text(11,75,paste('k = ',as.character(k)),col='black',pos=4,cex=1.5)
  text(11,60,TeX('$\\hat{k} = 8.89 \\pm 3.43$'),col='blue',pos=4,cex=1.5)
  text(11,45,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
  text(11,30,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('BinomEstpMoM181228.png',width=16,height=16,units='cm',res=300)
  hist(pEst,col='blue',breaks=33,las=1,main='',xlab=expression(hat(p)),cex.lab=1.5)
  abline(v=p,col='magenta')
  abline(v=pEstm,col='blue')
  text(0.2,50,paste('p = ',as.character(p)),col='magenta',pos=4,cex=1.5)
  text(0.6,50,TeX('$\\hat{k} = 0.506 \\pm 0.163$'),col='blue',pos=4,cex=1.5)
  text(0.73,40,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
  text(0.73,30,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  # dev.off()
}