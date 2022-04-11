print('file: SolutionAsymNoise.R')
# EXERCISE & SOLUTION: asymmetric noise
# ---------------------------------------------------
# (I) Construct asymmetric PDF with mean 0 for noise:
# 2 half uniform PDFs: PDF = b = 2/3 for 0 <= x <= 1; PDF = a = 1/6 for -2 <= x <= 0
# How to sample from this PDF? (1) random choise beween positive & negative branch
# (2) approriate uniform PDF either positive or negative branch
a = 1/6; b = 2/3   # PDF paramaters 
print(c(b/2/a,'b/(2*a)')) # ratio of branch areas
# ---------------------------------------------------
# II Test sampling
#  ---------------------------------------------------
set.seed(1953)
M = 1e5 # number Monte Carlo runs
rArr = numeric(M)
s = 0
for(i in 1:M) {r1 = sample(c(0,1,2),1,replace=TRUE); 
if (r1==0) {rArr[i] = runif(1,-2,0); s=s+1} else rArr[i] = runif(1,0,1)
}
meanEst = mean(rArr); print(c(round(meanEst,4),'meanEst'))
sdEst = sd(rArr); print(c(round(sdEst,4),'sdEst'))
varEst = var(rArr); print(c(round(varEst,4),'varEst'))
print(c(round(s/M,4),'s/M (should be close to 0.2)'))
sflag = 1
if (sflag == 1) {
  # png('ExerciseAsymPDF200923.png',width=16,height=16,units='cm',res=300)
  hist(rArr,main='',breaks=30,las=0,col='blue',xlab='z',cex.lab=1.5)
  text(-2,6500,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  text(-2,5500,paste('meanEst = ',as.character(round(meanEst,4))),col='blue',pos=4,cex=1.5)
  text(-2,4500,paste('varEst = ',as.character(round(varEst,4))),col='blue',pos=4,cex=1.5)
  # dev.off()
}