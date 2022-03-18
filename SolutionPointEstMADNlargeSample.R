print('file: SolutionPointEstMADNlargeSample.R')
# EXERCISE & SOLUTION: sample size depending MAD scaling: part (a)
#      MADN = Median Absolute deviation about the Median, Normalized
#      Moronna et al. (2006, p.~5)
# 
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 1000      # sample size
sdest = numeric(M)
for(m in 1:M) {x=rnorm(n); sdest[m]=median(abs(x-median(x)))/0.6745}
#  png('ExerMCmadnNormal160819.png',width=16,height=12,units='cm',res=300)
hist(sdest,30,col='blue',xlab='MADN',main='',las=1,cex.lab=1.5)
meansdest = mean(sdest); print(c(round(meansdest,3),'meansdest'))  # (expected: close to sigma = 1)
sdsdest = sd(sdest); print(c(round(sdsdest,3),'sdsdest'))
xt = 0.93
text(xt,95,'mean of estimate',col='blue',cex=1.5)
text(xt,85,paste('= ',as.character(round(meansdest,4))),col='blue',cex=1.5)
text(xt,75,'sd of estimate',col='blue',cex=1.5)
text(xt,65,paste('= ',as.character(round(sdsdest,4))),col='blue',cex=1.5)
text(1.07,95,paste('n = ',as.character(n)),col='black',cex=1.5)
text(1.07,85,paste('M = ',as.character(M)),col='black',cex=1.5)
# dev.off()
# ---------------------------------------------------
# Results: 
print(c('mean_sd_est = ',round(meansdest,4)))  # 0.9997
print(c('  sd_sd_est = ',round(sdsdest,4)))    # 0.0363