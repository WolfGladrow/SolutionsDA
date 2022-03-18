print('file: SolutionPointEstMADNscaling.R')
# EXERCISE & SOLUTION: sample size depending MAD scaling for small sample sizes
#      MADN = Median Absolute deviation about the Median, Normalized
#      Moronna et al. (2006, p.~5)
# write.table(meansdest,'MADNscale220304.txt')  #  M = 1e5
set.seed(1953)
# M = 1e4   # number of Monte-Carlo runs
M = 1e5   # number of Monte-Carlo runs
narr = seq(5,100); Ln = length(narr)  # sample sizes
meansdest = numeric(Ln); sdest = numeric(M)
for(k in 1:Ln) {n = narr[k];
for(m in 1:M) {x=rnorm(n); 
sdest[m]=median(abs(x-median(x)))}
meansdest[k] = mean(sdest)}
# png('ExerMCmadnNormalB160819.png',width=16,height=12,units='cm',res=300)    # M = 1e4
# png('ExerMCmadn1e5NormalB170220.png',width=16,height=12,units='cm',res=300) # M = 1e5
plot(narr,meansdest,type='p',lwd=3,col='blue',xlab='n',ylab='Average MAD estimate',las=1,
     cex=0.5,cex.lab=1.5,ylim=c(min(meansdest)/1.05,0.7))
xp = c(min(narr),max(narr)); yp = c(0.6745,0.6745)
lines(xp,yp,col='red',lty=2)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# For M = 1e5, calculation takes about 20 minutes! -> save results:
# write.table(narr,'narr220304.txt')            #  M = 1e5
# write.table(meansdest,'MADNscale220304.txt')  #  M = 1e5
# ----------------------------------------------------------------