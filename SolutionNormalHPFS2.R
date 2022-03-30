print('file: SolutionNormaHPFS2.R')
# EXERCISE & SOLUTION: exHPFS (part 2)
# (2) Normalize [y(x)]^10, construct a normal approximation, and plot both PDFs.
dx = 0.01; x = seq(0,pi,dx); y = sin(x); y10 = y^10
# (a) normalize y10:
y10N = sum(y10)*dx         # 0.7731263
y10normalized = y10/y10N
# (b) construct a normal approximation: calculate mean and variance of y10normalized
# (b1) mean mu is obviously pi/2, however, this is a code that can be applied
#      to other functions as well
mu1 = sum(x*y10normalized)*dx  # 1.570796 = pi/2 (as expected)
# (b2) variance:
var1 = sum( (x-mu1)^2*y10normalized)*dx   # 0.09066148
# (b3) normal density:
y10normalapprox = dnorm(x,mean=mu1,sd=sqrt(var1))
# (c) plot normalized y10 and normal approximation
# png('exHPFSM1603Part2.png',width=16,height=12,units='cm',res=300)
plot(x,y10normalized,type='l',lwd=2,col='blue',xlab='x',ylab='PDFs, difference',
     ylim=c(0,max(y10normalapprox)*1.03),las=1,cex.lab=1.5)
lines(x,y10normalapprox,col='red',lwd=2,lty=2)
d = y10normalapprox-y10normalized
lines(x,d,col='black',lwd=1,lty=1)
# dev.off()