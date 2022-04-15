print('file: SolutionAlphaBIC.R')
# EXERCISE & SOLUTION: alpha for AIC etc., Dziak et al. (2018, Table 2, p.12) 
AnAIC = 2
alphaAIC = 1-pchisq(AnAIC,1)
n = c(10,30,100,300,1e3,3000,1e4)
L = length(n)
alphaAICa = rep(alphaAIC,L)
alphaBIC = 1-pchisq(log(n),1)
alphaABIC = 1-pchisq(log((n+2)/24),1) 
# please note: for n = 10 is log((n+2)/24) < 0 -> pchisq = 0
alphaCAIC = 1-pchisq((log(n)+1),1)
# ------------
# png('Dziak18alphaEXERCISE181121.png',width=16,height=16,units='cm',res=300)
plot(log10(n),alphaBIC,type='p',lwd=4,col='blue',
     xlab=expression(paste(log[10],'(n)')),
     ylab=expression(alpha),las=1,cex=0.6,ylim=c(0,0.4),cex.lab=1.5)
points(log10(n),alphaAICa,col='black',pch=23,cex=0.6,lwd=4)
points(log10(n),alphaCAIC,col='red',pch=24,cex=0.6,lwd=4)
points(log10(n),alphaABIC,col='magenta',pch=22,cex=0.6,lwd=4)
text(0.95,0.02,'CAIC',col='red',pos=4,cex=1.5)
text(1.5,0.07,'BIC',col='blue',pos=4,cex=1.5)
text(3.6,0.2,'AIC',col='black',pos=4,cex=1.5)
text(1.8,0.3,'ABIC',col='magenta',pos=4,cex=1.5)
# dev.off()