print('file: SolutionPDsPDFsLognormNorm.R')
# EXERCISE & SOLUTION: plot lognormal & normal PDF: mu=409, sigma=250
LNparams = function(mu,sigma){
  # Calculate parameters alpha, beta for lognormal PDF from
  # mean mu > 0 and standard deviation sigma
  # DWG 7/2019 (based on function lnormal.params from bayescount)
  beta = sqrt(log(sigma^2/mu^2+1))
  alpha = log(mu) - ((beta^2)/2)
  return(c(alpha,beta))
}
mu=409; sigma=250
out = LNparams(mu,sigma)
alpha = out[1]; beta = out[2]
x = 1:1000
y = dlnorm(x,alpha,beta)
yn = dnorm(x,mu,sigma)
# png('LognormalNormal220301.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',lwd=3,col='blue',xlab='x',ylab='',las=1,cex=0.4,cex.lab=1.5)
lines(x,yn,col='red',lty=2)
legend('topright',legend=c('log-normal','normal'),col=c('blue','red'),lty=c(1,2),lwd=c(3,1))
# dev.off()