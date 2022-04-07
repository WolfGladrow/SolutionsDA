print('file: SolutionRobert07FairCoin.R')
# EXERCISE & SOLUTION: Robert07 fair coin p=1/2: n=16
n = 16; xarr = 0:n; L = length(xarr)
postH0arr = numeric(L)
B01arr = numeric(L)
print(c(n,'n'))
print(c('postH0','B01','x'))
for(j in 1:L) {
  x = xarr[j]
  postH0 = 1/(1+factorial(x)*factorial(n-x)*2^n/factorial(n+1))
  B01 = factorial(n+1)/(factorial(x)*factorial(n-x)*2^n)
  print(c(round(postH0,4),round(B01,4),x))
  postH0arr[j] = postH0
  B01arr[j] = B01
}
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('CoinPostH0x191014.png',width=16,height=16,units='cm',res=300)
  plot(xarr,postH0arr,type='p',lwd=4,col='blue',xlab='x',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
  title(ylab=TeX('Posterior for $H_0$ (not normalized)'),line=2.5,cex.lab=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('CoinB01x191014.png',width=16,height=16,units='cm',res=300)
  plot(xarr,B01arr,type='p',lwd=4,col='blue',xlab='x',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
  abline(h=10^(1/2),col='green',lty=1)
  abline(h=1,col='black',lty=2)
  abline(h=10^(-1/2),col='red',lty=4)
  title(ylab=TeX('Bayes factor $B_{01}$'),line=2.5,cex.lab=1.5)
  # dev.off()
}