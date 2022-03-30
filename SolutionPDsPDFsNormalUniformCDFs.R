print('file: SolutionPDsPDFsNormalUniformCDFs.R')
# EXERCISE & SOLUTION: CDFs for standard normal & standardized uniform PDF
x = seq(-3,3,0.01); CDFnormal = pnorm(x)                   # coarse resolution
b = sqrt(3); xu = c(-3,-b,b,3); CDFuniformN = c(0,0,1,1)
# find maximum y-difference between CDFs:
xd = seq(0,b,0.001); CDFn = pnorm(xd); CDFu = 0.5+0.5*xd/b # fine resolution
k = which.max(abs(CDFn-CDFu))  # 805
xDmax = xd[k]; print(c(round(xDmax,4),'xDmax')) 
Dmax = CDFn[k]-CDFu[k]; print(c(round(Dmax,4),'Dmax')) 
Dmaxr = round(Dmax,4)
xp = c(xDmax,xDmax); yp = c(CDFu[k],CDFn[k])
# png('CDFsNU160912.png',width=16,height=16,units='cm',res=300)
plot(x,CDFnormal,type='l',lwd=2,col='blue',xlab='x',ylab='CDF(x)',las=1,cex.lab=1.5)
lines(xu,CDFuniformN,col='black',lwd=2,lty=2)
lines(xp,yp,col='magenta',lwd=3)
xt = -1.5
text(xt,0.9,bquote(~D[max] == .(Dmaxr)),col='magenta',cex=1.5)
text(xt,0.7,paste('at x = ',as.character(round(xDmax,4))),col='magenta',cex=1.5)
# dev.off()