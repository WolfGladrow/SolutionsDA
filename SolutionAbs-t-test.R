print('file: SolutionAbs-t-test.R')
# EXERCISE & SOLUTION: t-test with modified test statistic a = abs(t) 
x = c(1.5,0.3,1.8,-1.4,0.8,3.0,-0.3,0.2,-0.4,1.9,0.0,0.3,-1.0,
      1.2,3.8,0.5,-0.8,2.0,1.1,1.2,-0.4,2.7,0.5,-1.4,1.1)
Tair = 0
xmean = mean(x); print(c(round(xmean,2),'sample mean (deg.C)'))
n = length(x); nu = n-1
SE = sd(x)/sqrt(n)
print(c(round(sd(x),2),'s = sd(x) (deg.C)'))
print(c(round(var(x),2),'s = var(x) (deg.C)^2'))
print(c(round(SE,4),'standard error of the mean (deg.C)'))
aobs = abs(xmean-Tair)/SE; aobsr = round(aobs,3)
print(c(round(aobs,2),'abs(xmean-mu0)/SE'))
avalue = abs(xmean-Tair)/SE
print(c(round(avalue,3),'avalue (pedestrian)'))
# p-value:
integrand = function(y) 2*dt(y,nu)
pvalue = integrate(integrand,lower=aobs,upper=Inf)$value
pr = round(pvalue,3)
# ---------------------------------------------------
aarr = seq(0,4,0.01)
faarr = 2*dt(aarr,nu)
alpha = 0.05
ac = -qt(alpha/2,nu); acr = round(ac,3)
print(c(round(ac,3),'ac'))
avalue = integrate(integrand,lower=ac,upper=Inf)$value
sflag = 1
if (sflag == 1) {
  # png('ExerciseAbst220404.png',width=16,height=16,units='cm',res=300)
  plot(aarr,faarr,type='l',lwd=3,col='black',
       xlab='a',ylab=NA,las=1,cex=0.4,xlim=c(0,3.8),
       ylim=c(0,0.84),xaxs='i',yaxs='i',cex.lab=1.5)
  title(ylab=TeX('$Half-Student-t(a;\\,\\nu = 24)$'),line=2.5,cex.lab=1.5)
  x1=aobs; x2=max(aarr); y1=2*dt(x1,nu); y2=2*dt(x2,nu); dx=(x2-x1)/50; 
  xn=seq(x1,x2,dx); yn=2*dt(xn,nu); xf=c(x2,x1,xn); yf=c(0,0,yn) 
  polygon(xf,yf,col='blue')
  text(0.5,0.1,bquote(~alpha == .(alpha)),col='red',pos=1,cex=1.5)
  text(0.5,0.2,bquote(~a[c] == .(acr)),col='red',pos=1,cex=1.5)
  text(0.5,0.3,bquote(~a[obs] == .(aobsr)),col='blue',pos=1,cex=1.5)
  text(0.5,0.4,bquote(~p == .(pr)),col='blue',pos=1,cex=1.5)
  abline(v=aobs,col='blue',lty=1)
  text(2.713,0.7,TeX('$a_{obs}$'),col='blue',pos=4,cex=1.5)
  text(2.713,0.04,as.character(pr),col='blue',pos=4,cex=1.5)
  abline(v=ac,col='red',lty=2)
  text(ac+0.1,0.7,TeX('$a_c$'),col='red',pos=4,cex=1.5)
  # dev.off()
}