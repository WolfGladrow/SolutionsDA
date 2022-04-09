print('file: SolutionPointEstAitkin.R')
# Aitkin dust counter
ja = seq(0,8)                    # number of events
frequencies=c(23,56,88, 95,73,40,17,5,3)   # frequencies
lambdaEst=sum(ja*frequencies)/sum(frequencies) # estimate of mean rate
# Bayesian estimate:
n = sum(frequencies)
s = sum(ja*frequencies)
lambdaEstB = (s+1)/n; print(c(round(lambdaEstB,4),'lambdaEstB'))
ulambdaEstB = sqrt((s+1))/n; print(c(round(ulambdaEstB,4),'ulambdaEstB'))
lambdaEstBr = round(lambdaEstB,3); ulambdaEstBr = round(ulambdaEstB,3)
# predictions:
pPredict=dpois(ja,lambdaEstB) # probabilities based on estimated lambda
relfre=frequencies/sum(frequencies) # relative frequencies
fPredict=pPredict*sum(frequencies)  # predicted frequencies
# ---------- plots:
sflag = 1
if (sflag == 1) {
  # png('DustCounterF170720.png',width=16,height=16,units='cm',res=300)
  plot(ja,frequencies,type='p',col='black',xlab='Number of events j',ylab='',
       main='',lwd=4,cex=0.6,las=1,cex.lab=1.5)
  title(ylab='Frequency',line=2.5,cex.lab=1.5)
  points(ja,fPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  text(2,10,pos=4,bquote(~bar(x) == .(lambdaEst)),col='magenta',cex=1.5)
  text(2,20,pos=4,bquote(~hat(lambda) == .(lambdaEstBr)  %+-% .(ulambdaEstBr)),col='magenta',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('DustCounterP170720.png',width=16,height=16,units='cm',res=300)
  plot(ja,relfre,type='p',col='black',las=0,cex=0.6,main='',xlab='Number of events j',
       ylab='',lwd=4,cex.lab=1.5)
  title(ylab='Probability, relative frequency',line=2.8,cex.lab=1.5)
  points(ja,pPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  text(2,0.025,pos=4,bquote(~bar(x) == .(lambdaEst)),col='magenta',cex=1.5)
  text(2,0.05,pos=4,bquote(~hat(lambda) == .(lambdaEstBr)  %+-% .(ulambdaEstBr)),col='magenta',cex=1.5)
  # dev.off()
}