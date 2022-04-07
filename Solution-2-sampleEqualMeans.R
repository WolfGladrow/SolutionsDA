print('file: Solution-2-sampleEqualMeans.R')
# Equal means: various 2-sample tests; data: Ott & Longnecker (2001, p. 272-274)
yd = c(18,43,28,50,16,32,13,35,38,33,6,7)
yu = c(40,54,26,63,21,37,39,23,48,58,28,39)
alpha = 0.05; print(c(alpha,'alpha'))
# ---------------------------------------------------------------------
pwilcox = wilcox.test(yd,yu)$p.value; print(c(round(pwilcox,4),'pwilcox'))
if(pwilcox < alpha) print(c('p < alpha = ',alpha,': reject H0'))
if(pwilcox >= alpha) print(c('p >= alpha = ',alpha,': do not reject H0'))
# ---------------------------------------------------------------------
pttest = t.test(yd,yu)$p.value; print(c(round(pttest,4),'pttest'))
if(pttest < alpha) print(c('pttest < alpha = ',alpha,': reject H0'))
if(pttest >= alpha) print(c('pttest >= alpha = ',alpha,': do not reject H0'))
# ---------------------------------------------------------------------
pttestv = t.test(yd,yu,var.equal=T)$p.value; print(c(round(pttestv,4),'pttestv (equal var)'))
if(pttestv < alpha) print(c('pttestv < alpha = ',alpha,': reject H0'))
if(pttestv >= alpha) print(c('pttestv >= alpha = ',alpha,': do not reject H0'))
# ---------------------------------------------------------------------
# Bayesian t-test:
library(BayesFactor)
outBF = ttestBF(yd,yu,rscale=1)
BF = extractBF(outBF,onlybf = TRUE); print(c(round(BF,4),'BF'))
if(BF > 10)                    print('strong evidence against H0/strong evidence for H1')
if((BF >= 3.16) && (BF <= 10)) print('substantial evidence against H0/substantial evidence for H1')
if((BF > 1) && (BF < 3.16))    print('slight evidence against H0/slight evidence for H1')
if((BF >= 0.316) && (BF <= 1)) print('slight evidence against H1/slight evidence for H0')
if((BF > 0.1) && (BF < 0.316)) print('substantial evidence against H1/substantial evidence for H0')
if(BF <= 0.1)                  print('strong evidence against H1/strong evidence for H0')
# ---------------------------------------------------------------------
sflag = 3
if (sflag == 3) {
  # png('WormsHist180325.png',width=16,height=16,units='cm',res=300)
  par(mar=c(4.1,4.5,1,1),mfrow=c(2,1))
  hist(yd,col='blue',las=1,main='',xlab='treated',cex.lab=1.5)
  hist(yu,col='blue',las=1,main='',xlab='untreated',cex.lab=1.5)
  # dev.off()
}