print('file: SolutionMLRhighNoise.R')
# EXERCISE & SOLUTION: MLR: low correlation, noise level approx. 0.4
beta0 = 1.5; beta1 = -0.3; beta2 = 0.8; beta3 = 1  # model parameters
n = 100   # sample size
cflag = 0     # 0 =  low correlation between x1 & x2
              # 1 = high correlation between x1 & x2
set.seed(1953) # set seed for random number generators
if (cflag == 0) x1 = runif(n); x2 = runif(n)
if (cflag == 1) {source('sampleUcorFct.R');
  r = -0.95; out1 = sampleUcorFct(r,n); 
  x1 = out1[1:n]; x2 = out1[(n+1):(2*n)]}
x1 = x1+4       # shift x1
x3 = runif(n)
yexact = beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
# noise level zNL: relative to range of yexact
zNL = 0.4*(max(yexact)-min(yexact))  # EXERCISE
yobserved = yexact+zNL*rnorm(n)
out2 = summary(lm(yobserved ~ x1+x2+x3))
b0 = out2$coefficients[1]   # estimate
b1 = out2$coefficients[2]
b2 = out2$coefficients[3]
b3 = out2$coefficients[4]
yfit = b0+b1*x1+b2*x2+b3*x3
ymin = min(c(min(yexact),min(yobserved),min(yfit)))
ymax = max(c(max(yexact),max(yobserved),max(yfit)))
# png('MLR3lowCor0d4NL161231.png',width=16,height=12,units='cm',res=300)
par(mfrow=c(1,3))
xp = seq(ymin,ymax,0.1)
plot(xp,xp,type='l',lwd=3,col='black',xlab='yexact',lty=3,
     ylab='yobserved',las=1,cex=0.5,xlim=c(ymin,ymax),ylim=c(ymin,ymax),cex.lab=1.5)
points(yexact,yobserved,col='blue',lwd=3,cex=0.5,lty=24)
plot(xp,xp,type='l',lwd=3,col='black',xlab='yexact',lty=3,
     ylab='yfit',las=1,cex=0.5,xlim=c(ymin,ymax),ylim=c(ymin,ymax),cex.lab=1.5)
points(yexact,yfit,col='blue',lwd=3,cex=0.5,lty=24)
plot(xp,xp,type='l',lwd=3,col='black',xlab='yfit',lty=3,
     ylab='yobserved',las=1,cex=0.5,xlim=c(ymin,ymax),ylim=c(ymin,ymax),cex.lab=1.5)
points(yfit,yobserved,col='blue',lwd=3,cex=0.5,lty=24)
# dev.off()