print('file: SolutionBrokenStickGeometric.R')
# EXERCISE & SOLUTION: broken stick vs. geometric distribution
n = 10; karr = seq(1,n); pk = seq(1,n); kinv = 1/karr;
for(k in 1:n) pk[k]=sum(kinv[k:n])/n
# geometric: h = probability of success in single trial
h = pk[1]; print(c(round(h,4),'h'))
k2 = seq(1,100) # number of trials until first success
pgeo = dgeom(k2-1,h)
di = pgeo[1:n]-pk
# png('BrokenStickGeo200830.png',width=16,height=16,units='cm',res=300)
plot(karr,pk,type='p',lwd=4,col='black',xlab='k',
     ylab='Probability, difference',las=1,cex=0.6,ylim=c(min(di),0.3),cex.lab=1.5)
points(k2,pgeo,lwd=4,col='magenta',cex=0.6,pch=24)
abline(h=0,col='green',lty=2)
points(karr,di,col='blue',lwd=4,cex=0.6,pch=22)
# dev.off()