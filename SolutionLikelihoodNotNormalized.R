print('file: SolutionLikelihoodNotNormalized.R')
# EXERCISE & SOLUTION: show by numerical integration that the normal likelihood function for 
# a single data point x1 = 0.8 is not normalized
NLfct = function(theta,x) {
  # normal likelihood function for a single observation x
  mu = theta[1]; sigma = theta[2];
  f = 1/(sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2))}
# install.packages('cubature')
library(cubature)
x1 = 0.8
muMax = 10; muMin = -muMax; sigMin = 0; sigMax = muMax # integration limits
out1=adaptIntegrate(NLfct,lowerLimit=c(muMin,sigMin),upperLimit=c(muMax,sigMax),x = x1)
muMaxa = seq(1,100); L = length(muMaxa)
z = numeric(L)
for(k in 1:L) {muMax = muMaxa[k];
   muMin = -muMax; sigMin = 0; sigMax = 20; # muMax;
   out1=adaptIntegrate(NLfct,lowerLimit=c(muMin,sigMin),upperLimit=c(muMax,sigMax),x = x1)
   z[k] = out1$integral}
NLfctN = function(theta,x,sigNorm) {
  # normalized (division by sigMax!) normal likelihood function
  mu = theta[1]; sigma = theta[2];
  f = 1/(sigNorm*sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2))}
zn = numeric(L)
eps = 1e-3
for(k in 1:L) {muMax = muMaxa[k];
   muMin = -muMax; sigMin = eps; sigMax = 20; # muMax;
   out1=adaptIntegrate(NLfctN,lowerLimit=c(muMin,sigMin),upperLimit=c(muMax,sigMax),
                    x = x1,sigNorm=sigMax)
   zn[k] = out1$integral}
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('NLFb181206.png',width=16,height=16,units='cm',res=300) # sigMax = 20
  plot(muMaxa,z,type='l',lwd=3,col='blue',xlab=TeX('$\\mu_{max}$'),
       ylab='Integral',las=1,cex=0.4,cex.lab=1.5)
  text(30,6,TeX('$\\sigma_{max} = 20$'),col='black',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('NLFNb181206.png',width=16,height=16,units='cm',res=300) # sigMax = 20
  plot(muMaxa,zn,type='l',lwd=3,col='blue',xlab=TeX('$\\mu_{max}$'),
       ylab='Integral (normalized)',las=1,cex=0.4,cex.lab=1.5)
  text(30,0.6,TeX('$\\sigma_{max} = 20$'),col='black',cex=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks: integration takes a few seconds
# adaptIntegrate() from package 'cubature' for multiple integration; here: 2D, over rectangle
# ----------------------------------------------------------------