print('file: SolutionOLSasymNoise.R')
# EXERCISE & SOLUTION: Ordinary least squares with non-normal asymmetric noise
# ---------------------------------------------------
# (I) Construct asymmetric PDF with mean 0 for noise:
# 2 half uniform PDFs: PDF = b = 2/3 for 0 <= x <= 1; PDF = a = 1/6 for -2 <= x <= 0
# How to sample from this PDF? (1) random choise beween positive & negative branch
# (2) approriate uniform PDF either positive or negative branch
a = 1/6; b = 2/3; sigmasq = 2/3
beta0 = 8; beta1 = -1.2 # true intercept & slope
csigma = 2; print(c(round(csigma,4),'csigma'))
sigma = csigma*sqrt(sigmasq); print(c(round(sigma,4),'sigma')) # true standard deviation of noise
set.seed(1953)
mMax = 5
mArr = 1:mMax
SL = numeric(mMax); IC = numeric(mMax)
print(' ---------------------------------------------------')
print('(II) Generate noise')
for(m in 1:mMax) {
  print(c(m,'m'))
  n = 10^m    # sample size
  rArr = numeric(n)
  for(i in 1:n) {r1 = sample(c(0,1,2),1,replace=TRUE); 
  if (r1==0) {rArr[i] = runif(1,-2,0)} else rArr[i] = runif(1,0,1)}
  x = round(sort(runif(n,min=1,max=5)),2)
  xExact = seq(1,5,0.1)
  yExact = beta0+beta1*xExact
  y = beta0+beta1*x+csigma*rArr
  out1 = lm(y~x)
  IC[m] = out1$coefficients[1]
  SL[m] = out1$coefficients[2]
}
sflag = 1
if (sflag == 1) {
  # png('Gauss-MarkovSlope200925.png',width=16,height=12,units='cm',res=300)
  plot(mArr,SL,type='p',lwd=4,col='blue',xlab='m',ylab='Slope',las=1,cex=0.6,cex.lab=1.5)
  abline(h=beta1,col='green',lty=4)
  points(mArr,SL,lwd=4,col='blue',cex=0.6)
  # dev.off()
}
if (sflag == 2) {
  # png('Gauss-MarkovIntercept200925.png',width=16,height=12,units='cm',res=300)
  plot(mArr,IC,type='p',lwd=4,col='blue',xlab='m',ylab='Intercept',las=1,cex=0.6,cex.lab=1.5)
  abline(h=beta0,col='green',lty=4)
  points(mArr,IC,lwd=4,col='blue',cex=0.6)
  # dev.off()
}