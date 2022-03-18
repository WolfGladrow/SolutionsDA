print('file: SolutionPDsPDFsNegBinoLogNeg.R')
# EXECISE & SOLUTION: Negative Binomial versus Log-Negative PDF
# conversion between (mu,sigma) and (s,p)
LNparams = function(mu,sigma){
  # Calculate parameters alpha, beta for lognormal PDF from
  # mean mu > 0 and standard deviation sigma
  # DWG 7/2019 (based on function lnormal.params from bayescount)
  beta = sqrt(log(sigma^2/mu^2+1))
  alpha = log(mu) - ((beta^2)/2)
  return(c(alpha,beta))
}
mu = 409; sigma = 250
# mu = s*(1-p)/p and sigma^2 = mu+mu^2/s
# -> s = mu^2/(sigma^2-mu) and p*mu = s - s*p -> p = s/(mu+s)
s = mu^2/(sigma^2-mu); p = s/(mu+s)
mu1 = s*(1-p)/p; sigma1 = sqrt(mu+mu^2/s)
print(c(round(p,4),'p'))
print(c(round(s,4),'s'))
print(c(round(mu1,1),'mu1'))
print(c(mu,'mu'))
print(c(round(sigma1,1),'sigma1'))
print(c(sigma,'sigma'))
out = LNparams(mu,sigma)
alpha = out[1]; beta = out[2]
x = seq(1,1000); LN = dlnorm(x,alpha,beta)
xNB = seq(1,1000,10); NB = dnbinom(xNB,s,p)
# png('NegativeBinomialVsLogNeg190723.png',width=16,height=12,units='cm',res=300)
plot(x,LN,type='l',lwd=3,col='blue',xlab='x',ylab='',las=1,cex=0.4,cex.lab=1.5)
points(xNB,NB,col='red',lwd=3,cex=0.4)
# dev.off()