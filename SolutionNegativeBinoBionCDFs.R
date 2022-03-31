print('file: SolutionNegativeBinoBinoCDFs.R')
# EXERSISE: siblings, CDFs of bin., neg. bin. & normal dist.
mu = 6; sigmasq = 3.2308; sigma = sqrt(sigmasq)
pb = 1 - sigmasq/mu; n = mu/pb # binomial
print(c('n (before rounding) = ',n))
n = round(n); na = seq(0,n); pbinomial = dbinom(na,n,pb)
print(c('n = ',n))
print(c('pb = ',round(pb,4)))
p = sigmasq/mu; s = mu*p/(1-p)  # negative binomial
print(c('s (before rounding) = ',s))
s = round(s); pnegbino = dnbinom(na,s,p)
print(c('s = ',s))
print(c('p = ',round(p,4)))
na2 = seq(0,mu+5*sigma,0.1)
# png('BinoNegBinoCDFs220225.png',width=16,height=16,units='cm',res=300)
plot(na2,pbinom(na2,n,pb),type='l',lwd=1,col='blue',xlab='k or x',ylab='CDF',
     las=1,cex.lab=1.5)
lines(na2,pnbinom(na2,s,p),col='magenta',lwd=1)
lines(na2,pnorm(na2,mean=mu,sd=sigma),col='black')
# dev.off()