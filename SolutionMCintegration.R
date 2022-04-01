print('file: SolutionMCintegration.R')
# EXERCISE & SOLUTION: Monte Carlo integration
# Robert & Casella (2009, p.65-67, modified)
set.seed(1953) # set seed for random number generators
h=function(x){(cos(23*x)+sin(38*x))^2}
a = 38; b = 23
Ianalytic = 1-sin(2*a)/(4*a)+sin(2*b)/(4*b)-
  cos(a-b)/(a-b)-cos(a+b)/(a+b)+1/(a-b)+1/(a+b) # 1.144015
# png('MCintEXERCISE160811.png',width=16,height=12,units='cm',res=300)
out1 = integrate(h,0,1) # 1.144015 with absolute error < 2e-05
Inumeric = out1$value   #  1.144015
par(mar=c(4.1,4.5,1,1),mfrow=c(2,1))
curve(h,ylab='Function',xlab='x',lwd=2,las=1,col='blue',cex.lab=1.5)
x=h(runif(10^4))
estint=cumsum(x)/(1:10^4)
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint, ylab='Estimate',type='l',las=1,lwd=2,
     xlab='n',ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),cex.lab=1.5)
lines(estint+2*esterr,col='gold',lwd=2)
lines(estint-2*esterr,col='gold',lwd=2)
xp = c(0,10000); yp = c(Ianalytic,Ianalytic)
lines(xp,yp,col='red',lty=2)
# dev.off()