print('file: SolutionPDsPDFsNormalProperties.R')
# Generate a graph that displays the standard normal PDF with some 
#     of its properties: $\mu$, $\sigma$, maximum value, 
#     areas for ranges $\pm n\, \sigma$ with $n = 1,2,3$, 66\%, 95\% and 99\% level.
library(latex2exp)
x = seq(-3,3,0.01); y = dnorm(x)
# png('exPropNormal220225.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',xlab='x',ylab='Standard normal distribution',col='blue',lwd=3,las=1) 
text(-2,0.35,TeX('$\\mu = 0$'),col='blue',cex=1.5)
text(-2,0.3,TeX('$\\sigma = 1$'),col='blue',cex=1.5)
q = 0.99              # find 99% level:
x99 = qnorm((1-q)/2)  #   -> search 1/2% of the cumulative probability distribution
xp99 = c(x99,x99); yp99 = c(0,dnorm(x99))  
lines(xp99,yp99,col='red',lty=2,lwd=2)
lines(-xp99,yp99,col='red',lty=2,lwd=2)
text(-2.55,0.1,TeX('$x_{99} = $'),col='red',cex=1.5)
text(-2.56,0.06,'-2.576',col='red',cex=1.5)
q = 0.95              # find 95% level:
x95 = qnorm((1-q)/2)  #   -> search 5/2% of the cumulative probability distribution
xp95 = c(x95,x95); yp95 = c(0,dnorm(x95))
lines(xp95,yp95,col='black',lty=2,lwd=2)
lines(-xp95,yp95,col='black',lty=2,lwd=2)
text(-2.3,0.15,TeX('$x_{95} = -1.960$'),col='black',cex=1.5)
q = 0.67              # find 67% level:
x67 = qnorm((1-q)/2)  #   -> search 33/2% of the cumulative probability distribution
xp67 = c(x67,x67); yp67 = c(0,dnorm(x67))
lines(xp67,yp67,col='magenta',lty=2,lwd=2)
lines(-xp67,yp67,col='magenta',lty=2,lwd=2)
text(-2.1,0.2,TeX('$x_{67} = -0.974$'),col='magenta',cex=1.5)
ymax = max(y)   # 0.3989423
text(1.5,0.38,'max = 0.3989',col='blue',cex=1.5)
# dev.off()