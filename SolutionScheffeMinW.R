print('file: SolutionScheffeMinW.R')
# EXERCISE & SOLUTION: Scheffe CB minimum width (n)
n = seq(10,100,10)
alpha = 0.1
Malpha = sqrt(2*qf(1-alpha,2,n-2))
wmin = 2*Malpha/sqrt(n)
xp = seq(10,100)
yp = wmin[1]*sqrt(xp[1])/sqrt(xp)
sflag = 1
if (sflag == 1) {
  # png('ScheffeExMinWidth210116.png',width=16,height=16,units='cm',res=300)
  plot(n,wmin,type='p',lwd=4,col='blue',xlab='n',ylab='Minimum width',las=1,cex=0.6,cex.lab=1.5)
  lines(xp,yp,col='black',lty=4)
  # dev.off()
}