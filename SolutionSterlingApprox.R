print('file: SolutionSterlingApprox.R')
# EXERCISE & SOLUTION: How good is the Stirling approximation? (10/2017)')
n = seq(10,500); L = length(n)
# lognfe = log(factorial(n))  # exact DOES NOT WORK FOR LARGE n (> 150)
z2 = numeric(L)
for(j in 1:L) {n2 = n[j]; s = 0;
for(k in 1:n2) s=s+log(k);
z2[j] = s}
Stir = n*log(n) - n        # approximation
r2 = Stir/z2
j99 = 0
for(j in 1:L) if(r2[j] < 0.99) j99 = j
n99 = n[j99]
sflag = 2
if (sflag == 2) {
  # png('Stirling171001.png',width=16,height=12,units='cm',res=300)
  plot(n,r2,type='p',lwd=3,col='blue',xlab='n',ylab='r',las=1,cex=0.4,cex.lab=1.5)
  abline(0.99,0,col='green')
  text(90,0.97,paste('r(n=90) = ',as.character(round(r2[j99+1],3))),col='blue',pos=4,cex=1.5)
  # dev.off()
}