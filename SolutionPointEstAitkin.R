print('file: SolutionPointEstAitkin.R')
# Aitkin dust counter
ja = seq(0,8)                    # number of events
frequencies=c(23,56,88, 95,73,40,17,5,3)   # frequencies
lambdaEst=sum(ja*frequencies)/sum(frequencies) # estimate of mean rate
pPredict=dpois(ja,lambdaEst) # probabilities based on estimated lambda
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
  text(5.6,80,pos=4,bquote(~hat(lambda) == .(lambdaEst)),col='magenta',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('DustCounterP170720.png',width=16,height=16,units='cm',res=300)
  plot(ja,relfre,type='p',col='black',las=0,cex=0.6,main='',xlab='Number of events j',
       ylab='',lwd=4,cex.lab=1.5)
  title(ylab='Probability, relative frequency',line=2.8,cex.lab=1.5)
  points(ja,pPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  text(5.6,0.2,pos=4,bquote(~hat(lambda) == .(lambdaEst)),col='magenta',cex=1.5)
  # dev.off()
}
# ---------------------------------------------------
# Results: 
TNOC = sum(frequencies)        # total number of cases
TNOD = sum(ja*frequencies)     # total number of deaths
print(c('total number of cases  = ',TNOC))
print(c('total number of deaths = ',TNOD))
print(c('estimate of mean rate  = ',lambdaEst))
print(c('probabilities based on estimated lambda  = ',pPredict))
print(c('probabilities based on estimated lambda  = ',round(pPredict,4)))
print(c('relative frequencies  = ',round(relfre,4)))
print(c('predicted frequencies = ',round(fPredict,4)))