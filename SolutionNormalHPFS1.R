print('file: SolutionNormalHPFS1a.R')
# EXERCISE & SOLUTION: exHPFSM1603 (part 1)
# Jaynes (2003, p.~189) remarked that 'any function with a single rounded 
# maximum raised to a higher and higher power, goes into a Gaussian function'. 
# $y(x) = \sin x$ in the range $0 \le x \le \pi$ is such a function. 
# (1) Plot $y(x)$, $\left[ y(x) \right]^5$, and $\left[ y(x) \right]^{10}$.
dx = 0.01; x = seq(0,pi,dx); y = sin(x); y5 = y^5; y10 = y^10
y10Mean = sum(x*y10)/sum(y10); print(c(round(y10Mean,4),'y10Mean')) # pi/2 (as expected)
y10Var = sum((x-y10Mean)^2*y10)/sum(y10); print(c(round(y10Var,4),'y10Var')) # 0.0907
y10Sd = sqrt(y10Var); print(c(round(y10Sd,4),'y10Sd')) # 0.3011
# normal approximation:
yn = dnorm(x,y10Mean,y10Sd)
# rescale
yn = yn/max(yn)
library(latex2exp)
# png('exHPFSM220226Part1.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=1,col='black',xlab='x',ylab=NA,
     xlim=c(0,pi),ylim=c(0,1.05),las=1,lty=2,cex.lab=1.5)
title(ylab=TeX('$(sin(x))^n$'),line=2.3,cex.lab=1.5)
lines(x,y5,col='magenta',lwd=1,lty=3); lines(x,y10,col='blue',lwd=3)
lines(x,yn,col='red',lwd=3,lty=4)
text(0.4,0.6,'n = 1',col='black',cex=1.5)
text(0.75,0.4,'n = 5',col='magenta',cex=1.5)
text(1.3,0.2,'n = 10',col='blue',cex=1.5)
text(pi/2,0.01,'normal approximation',col='red',cex=1.5)
# dev.off()