print('file: SolutionMADN5scaled.R')
# ---------------------------------------------------------
# Estimate spread: MADN variable scaling (8/2016)')
#      MADN = Median Absolute deviation about the Median, Normalized
#      Moronna et al. (2006, p.~5)
sflag = 1
if (sflag == 1) {
  # first create data (may take a few minutes) and write to file:
set.seed(1953)
M = 1e5   # number of Monte-Carlo runs
narr = seq(5,100); Ln = length(narr)  # sample sizes
meansdest = numeric(Ln); sdest = numeric(M)
for(k in 1:Ln) {n = narr[k];
for(m in 1:M) {x=rnorm(n); sdest[m]=median(abs(x-median(x)))}
   meansdest[k] = mean(sdest)}
write.table(meansdest,'MADNscale220406.txt')
}
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size; n = 5 is minimum sample size
if (n < 100) {
    out1 = read.table('MADNscale220406.txt')
    scale1 = out1$x[n-4]
}
  print(c(round(scale1,4),'scale1'))
  sdest = numeric(M)
  for(m in 1:M) {x=rnorm(n); sdest[m]=median(abs(x-median(x)))/scale1}
# png('MADNn5Scaling160819.png',width=16,height=16,units='cm',res=300)
  hist(sdest,30,col='blue',xlab='MAD/0.5564',main='',las=1,cex.lab=1.5)
  meansdest = mean(sdest); print(c(round(meansdest,4),'meansdest'))  #   (expected: close to sigma = 1)
  sdsdest = sd(sdest); print(c(round(sdsdest,4),'sdsdest'))
  abline(v=1,col='black',lty=1)
  abline(v=meansdest,col='blue',lty=4)
  xt = 1.5
  text(xt,65,pos=4,paste('mean of estimate = ',as.character(round(meansdest,2))),col='blue',cex=1.5)
  text(xt,55,pos=4,paste('sd of estimate = ',as.character(round(sdsdest,2))),col='blue',cex=1.5)
  text(2.5,25,paste('n = ',as.character(n)),col='black',cex=1.5,pos=4)
  text(2.5,15,paste('M = ',as.character(M)),col='black',cex=1.5,pos=4)
# dev.off()