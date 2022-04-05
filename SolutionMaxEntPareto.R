print('file: SolutionMaxEntPareto.R')
# EXERCISE & SOLUTION: MaxEnt Pareto PDF
alpha = 0.6
xm = 1.5
x = seq(xm,10,0.01)
p = alpha*xm^alpha/x^(alpha+1)
myFct1 = function(z){alpha*xm^alpha/z^(alpha+1)}
myFct2 = function(z){log(z)*alpha*xm^alpha/z^(alpha+1)}
C1 = integrate(myFct1,xm,Inf)  # normalization
C2 = integrate(myFct2,xm,Inf)  # should be log(xm) + 1/alpha
C2ana = log(xm) + 1/alpha
# xlab=TeX('$\\lambda_2$'),
library(latex2exp)
# png('ParetoPD170214.png',width=16,height=16,units='cm',res=300)
plot(x,p,type='l',lwd=4,col='blue',xlab='x',ylab='Pareto PDF',las=1,cex.lab=1.5)
xt = 3
text(xt,0.3,bquote(~x[m] == .(xm)),col='blue',pos=4,cex=1.5)
text(xt,0.2,bquote(~alpha == .(alpha)),col='blue',pos=4,cex=1.5)
# dev.off()