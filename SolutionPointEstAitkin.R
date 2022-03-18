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
  # png('DustCounterF170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,frequencies,type='p',col='red',xlab='Number of events j',ylab='',
       main='',lwd=3,cex=0.5,las=1,cex.lab=1.5)
  title(ylab='Frequencies',line=2.5,cex.lab=1.5)
  points(ja,fPredict,col='blue',lwd=2,cex=0.5)
  text(5.6,80,pos=4,expression(paste(hat(lambda))),col='red',cex=1.5)
  text(6.0,80,pos=4,paste('= ',as.character(round(lambdaEst,3))),col='red',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('DustCounterP170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,relfre,type='p',col='red',las=1,cex=0.5,main='',xlab='Number of events j',
       ylab='',lwd=3,cex.lab=1.5)
  title(ylab='Probabilities,relative frequencies',line=2.8,cex.lab=1.5)
  points(ja,pPredict,col='blue',lwd=2,cex=0.5)
  text(5.6,0.2,pos=4,expression(paste(hat(lambda))),col='red',cex=1.5)
  text(6.0,0.2,pos=4,paste('= ',as.character(round(lambdaEst,3))),col='red',cex=1.5)
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