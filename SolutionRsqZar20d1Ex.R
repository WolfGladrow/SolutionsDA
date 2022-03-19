print('file: SolutionRsqZar20d1Ex.R')
# EXERCISE & SOLUTION: coefficient of determination (R^2); Zar (2010, Example 20.1) 
# install.packages('MuMIn')
library(MuMIn)
X1 = c(6,1,-2,11,-1,2,5,1,1,3,11,9,5,-3,1,8,-2,3,6,10,4,5,5,3,8,8,6,6,3,5,1,8,10)
X2 = c(99,93,94,91,69,93,79,74,73,88,98,105,91,101,72,117,87,76,
       86,109,76,73,92,70,72,70,88,101,121,77,78,115,104)/10
X3 = c(57,64,57,61,60,57,59,62,55,52,57,61,64,55,55,60,55,62,
       59,56,58,58,52,60,55,64,62,54,54,62,68,62,64)/10
X4 = c(16,30,34,34,30,44,22,22,19,2,42,24,34,30,2,39,22,44,2,24,
       24,44,16,19,16,41,19,22,41,16,24,19,22)/10
Y = c(212,339,361,172,180,321,259,325,286,232,157,150,269,406,
      198,229,355,331,183,169,242,298,184,248,283,241,178,
      222,272,236,281,164,182)/100 
Mfull = lm(Y ~ X1+X2+X3+X4)
options(na.action = 'na.fail') # change the default 'na.omit' to prevent models
# from being fitted to different datasets in case of missing values.
MdfullAIC = dredge(Mfull,rank='AIC')
n = length(Y)
print(' ---------------------------------------------------')
print('Exercise: calculate coefficient of determination for all models:')
slopeX1 = MdfullAIC$X1; slopeX2 = MdfullAIC$X2
slopeX3 = MdfullAIC$X3; slopeX4 = MdfullAIC$X4
ic = MdfullAIC$`(Intercept)`
# All 16 models except model 1 (k == 13):
LM = 16; Rsqa = numeric(LM)
for(k in 1:LM) {
  yp = rep(ic[k],n);
  if (is.na(slopeX1[k]) == FALSE) yp = yp+slopeX1[k]*X1;
  if (is.na(slopeX2[k]) == FALSE) yp = yp+slopeX2[k]*X2;
  if (is.na(slopeX3[k]) == FALSE) yp = yp+slopeX3[k]*X3;
  if (is.na(slopeX4[k]) == FALSE) yp = yp+slopeX4[k]*X4;
  if ((k == 13) == FALSE) Rsqa[k] = (cor(yp,Y))^2
}
print(c(round(Rsqa,4),' Rsqa'))
ModelNumber = c(10,12,14,16,2,6,4,8,11,15,9,13,1,3,5,7)
sflag = 1
if (sflag == 1) {
  # png('RsqZar20d1Ex220319.png',width=16,height=12,units='cm',res=300)
  plot(ModelNumber,Rsqa,type='p',lwd=4,col='blue',xlab='Model #',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
  title(ylab=TeX('$R^2$'),cex.lab=1.5,line=2.5)
  # dev.off()
}