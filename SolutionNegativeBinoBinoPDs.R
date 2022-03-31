print('file: SolutionNegativeBinoBinoPDs.R')
# EXERSISE: siblings, PDFs of bin., neg. bin. & normal dist.
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
# png('BinoNegBinoPDs180720.png',width=16,height=16,units='cm',res=300)
plot(na,pbinomial,type='p',lwd=4,col='black',xlab='k',ylab='Probability',las=1,cex=0.6,cex.lab=1.5)
points(na,pnegbino,col='magenta',lwd=4,cex=0.6,pch=24)
# dev.off()