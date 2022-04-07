print('file: Solution-VRTcount.R')
# EXERCISE & SOLUTION: variance ratio test for count data (Monte Carlo simulation)
set.seed(1953)
m=11; n=10 #   sample sizes
x1min = 10; x2min = 33; x1x2range = 16
my.Fsimulation=function(){ # generate random samples from discrete uniform PD
  xrange1 = seq(x1min,x1min+x1x2range); xrange2 = seq(x2min,x2min+x1x2range);
  z1=sample(x=xrange1,m,replace=TRUE); z2=sample(x=xrange2,n,replace=TRUE);
  F.stats = var(z1)/var(z2)}
Fstat.vector=replicate(1e6,my.Fsimulation())
out = density(Fstat.vector,from=0,to=3)
xd = out$x; yd = out$y; yF = df(xd,df1=m-1,df2=n-1)
dx = xd[2]-xd[1]  # equidistant 
Fobsa = seq(0.099804305,0.5,dx); LF = length(Fobsa)
pMCa = numeric(LF)
pa = numeric(LF)
for(j in 1:LF) {Fobs = Fobsa[j]; k = which.min((xd-Fobs)^2);
   pMCa[j] = 2*dx*sum(yd[1:k]);
   pa[j] = 2*pf(Fobs,df1=m-1,df2=n-1)
}
sflag = 1
if(sflag == 1) {
  # png('VarTextExer171225.png',width=16,height=16,units='cm',res=300)
  plot(Fobsa,pa,type='l',lwd=4,col='blue',xlab='F',ylab='p, pMC',las=1,
       cex.lab=1.5,lty=4)
  abline(a=0.05,b=0,col='green',lty=2)
  lines(Fobsa,pMCa,col='black',lwd=3)
  legend('topleft',c('Monte Carlo','F(x;10,9)'),lwd=c(3,3),
         col=c('black','blue'),lty=c(1,4),cex=1.5)
  # dev.off()
}
alpha = 0.05
kpMC = which.min((pMCa-alpha)^2)
kp   = which.min((pa-alpha)^2)
print(c('Fobsa[kpMC] = ',round(Fobsa[kpMC],4)))  # 0.3816
print(c('Fobsa[kp]   = ',round(Fobsa[kp],4)))    # 0.2642
# ----------------------------------------------------------------
# Remarks:
# Monte-Carlo simulation takes less than 2 minutes
# ----------------------------------------------------------------