print('file: SolutionUncertaintyPoVfxy.R')
# EXERCISE & SOLUTION: propagation of variances: Monte Carlo & robust, f(x,y) = x*y;
#      x = 2.1 +- 0.5; y = 1.5 +- 0.3
x = 2.1; sx = 0.5; y = 1.5; sy = 0.3
# (a) law of propagation of uncertainty:
dfdx = y; dfdy = x
ssqfana = dfdx^2*sx^2+dfdy^2*sy^2; print(c(round(ssqfana,4),'ssqfana'))
sfana = sqrt(dfdx^2*sx^2+dfdy^2*sy^2); print(c(round(sfana,4),'sfana'))
f = x*y; print(c(round(f,4),'f'))   # 3.15
# (b) Monte Carlo:
set.seed(1953) # set seed for random number generators
M = 1e4        # number of Monte Carlo runs
rx=rnorm(M,mean=x,sd=sx)
ry=rnorm(M,mean=y,sd=sy)
rf = rx*ry
# (b1) non-robust estimate of central tendency & dispersion:
meanMC = mean(rf); print(c(round(meanMC,4),'meanMC'))  # arithmetic mean of sample 
sfMC = sd(rf); print(c(round(sfMC,4),'sfMC'))          # standard deviation 
# (b2) robust estimate of central tendency & dispersion:
medianMC = median(rf); print(c(round(medianMC,4),'medianMC'))
#  Normalized Median Absolute Deviation (MADN) estimate:
sfMCrobust= median(abs(rf-median(rf)))/0.6745; print(c(round(sfMCrobust,4),'sfMCrobust'))
# png('MCxyDensity161204.png',width=16,height=12,units='cm',res=300)
  plot(density(rf,from=0.5,to=6.5),type='l',col='blue',main='',las=1,
       xlab='f(rx,ry) = rx*ry',lwd=3,xlim=c(1,6),cex.lab=1.5)
  xp = seq(0,15,0.01)
  ypme = dnorm(xp,mean=medianMC,sd=sfMCrobust)
  lines(xp,ypme,col='black',lty=4,lwd=1)
  yp = dnorm(xp,mean=f,sd=sfana)
  lines(xp,yp,col='red',lty=2,lwd=2)
  text(3,0.2,paste('M = ',as.character(M)),col='blue',cex=1.5,pos=1)
  legend('bottom',legend=c('MC estimate','normal (MC mean)','normal (analytic mean)'),
         col=c('blue','black','red'),
         lty=c(1,4,2),lwd=c(3,1,2))
# dev.off()