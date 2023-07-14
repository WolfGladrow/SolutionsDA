print('file: SolutionDiabetesRidge.R')
# created by: Dieter.Wolf-Gladrow@awi.de 7/2023
# based on Efron & Hastie (2021)
Q = read.csv('diabetes.csv',header=TRUE)
names1 = names(Q)
n = dim(Q)[1] # sample length
p2 = dim(Q)[2] # number of predictors + 2 (1 for row numbers & 1 for response y)
p = p2-2 # number of predictors
X = matrix(data=NA,nrow=n,ncol=p) # Structure matrix X (n times p):
for(i in 1:n) for(j in 1:p) X[i,j] = Q[i,j+1]
C = round(cor(X),2) # Correlation matrix C
# -------- Multi-linear regression (MLR) using lm(): 
y = Q$prog
MLR1 = lm(y ~ X)
round(coef(summary(MLR1)),2)
# --------- Multi-linear regression (MLR): pedestrian way
# --------- Extend design matrix by adding a column of 1 for intercept beta0:
p1 = p+1
X1 = matrix(data=NA,nrow=n,ncol=p1)
for(i in 1:n) X1[i,1] = 1
for(i in 1:n) for(j in 2:p1) X1[i,j] = Q[i,j]
S =  t(X1)%*%X1
P = solve(S) # inverse of S
betaMLR = P%*%t(X1)%*%y
print(c(round(t(betaMLR),2),'betaMLR (not standardized)')) 
# ----------------------------------------------------------
# ----------------------------------------------------------------------
# Ridge regression: 
#.  requires standardization which consists of 
#   removing the sample mean followed by scaling with 
#   (a) the standard deviation or
#   (b) the square root of the sum of squares (called 'unit length scaling')
# ----------------------------------------------------------------------
# ----- Try reproduce values in Table 7.3 for lambda = 0.1
lambda = 0.1
# ----- (a) standard deviation scaling (SDS)
sj = numeric(p)      # scaling factors
X2 = matrix(NA,n,p)
for(j in 1:p) {
  a = X[1:n,j]; amean = mean(a);
  avar = var(a)*(n-1)/n  # std-scaling (Efron & Hastie, 2021, Eq. 7.58)
  sj[j] = sqrt(avar)
  X2[1:n,j] = (a-amean)/sj[j]
}
# Identity matrix: I
I = diag(p)
# I = matrix(data=NA,nrow=10,ncol=10)            # pedestrian
# for(i in 1:p) for(j in 1:p) I[i,j] = 0       # pedestrian
# for(i in 1:p) I[i,i] = 1                      # pedestrian
# ----
S = t(X2)%*%X2
P = solve(S+lambda*I)
betaSDS = P%*%t(X2)%*%y
print(c(round(t(betaSDS),2),'betaSDS(lambda=0.1)'))
# ----- (b) unit length scaling
X3 = matrix(NA,n,p)
for(j in 1:p) {
  a = X[1:n,j]; amean = mean(a);
  avar = var(a)*(n-1)  # unit length scaling (ULS)
  sj[j] = sqrt(avar)
  X3[1:n,j] = (a-amean)/sj[j]
}
# ----
S = t(X3)%*%X3
P = solve(S+lambda*I)
betaULS = P%*%t(X3)%*%y
print(c(round(t(betaULS),2),'betaULS(lambda=0.1)'))
# -----------------------------------------------------------------------
# Now vary lambda:
lambdaArr = seq(0,0.25,0.01); LL = length(lambdaArr)
M = matrix(data=NA,nrow=p,ncol=LL)
for(m in 1:LL) {
  lambda = lambdaArr[m]
  P = solve(S+lambda*I)
  betaULS = P%*%t(X3)%*%y
  for(j in 1:10) M[j,m] = betaULS[j]
}
sflag = 1
if (sflag == 1) { # try reproduce Fig. 7.2
  # install.packages('latex2exp')
  library(latex2exp)
  # png('Efron21HastieRidgeFig7d2m230713.png',width=16,height=16,units='cm',res=300)
  plot(lambdaArr,M[1,1:LL],type='l',lwd=2,col='black',
       xlab=NA,ylab=NA,las=1,cex=0.3,cex.lab=1.5,
       xlim=c(-0.05,0.25),ylim=c(-800,800))
  title(xlab=TeX('$\\lambda$'),cex.lab=1.5)
  title(ylab=TeX('$\\hat{\\beta}(\\lambda)$'),cex.lab=1.5,line=2)
  abline(h=0,col='magenta',lty=4)
  points(lambdaArr,M[1,1:LL],lwd=2,cex=0.3,col='black') 
  lines(lambdaArr,M[2,1:LL],lwd=2,cex=0.3,col='orange') 
  points(lambdaArr,M[2,1:LL],lwd=2,cex=0.3,col='orange')
  lines(lambdaArr,M[3,1:LL],lwd=2,cex=0.3,col='turquoise') 
  points(lambdaArr,M[3,1:LL],lwd=2,cex=0.3,col='turquoise')
  lines(lambdaArr,M[4,1:LL],lwd=2,cex=0.3,col='green') 
  points(lambdaArr,M[4,1:LL],lwd=2,cex=0.3,col='green') 
  lines(lambdaArr,M[5,1:LL],lwd=2,cex=0.3,col='black') 
  points(lambdaArr,M[5,1:LL],lwd=2,cex=0.3,col='black') 
  lines(lambdaArr,M[6,1:LL],lwd=2,cex=0.3,col='red') 
  points(lambdaArr,M[6,1:LL],lwd=2,cex=0.3,col='red') 
  lines(lambdaArr,M[7,1:LL],lwd=2,cex=0.3,col='blue') 
  points(lambdaArr,M[7,1:LL],lwd=2,cex=0.3,col='blue') 
  lines(lambdaArr,M[8,1:LL],lwd=2,cex=0.3,col='darkgreen') 
  points(lambdaArr,M[8,1:LL],lwd=2,cex=0.3,col='darkgreen') 
  lines(lambdaArr,M[9,1:LL],lwd=2,cex=0.3,col='brown') 
  points(lambdaArr,M[9,1:LL],lwd=2,cex=0.3,col='brown') 
  lines(lambdaArr,M[10,1:LL],lwd=2,cex=0.3,col='violet') 
  points(lambdaArr,M[10,1:LL],lwd=2,cex=0.3,col='violet') 
  for(j in 1:10) text(-0.04,M[j,1],names1[j+1],col='black',pos=4)
  # dev.off()
}
# -----------------------------------------------------------------
# Reduce number of predictors: apply dredge()
A1 = X[,1]; A2 = X[,2]; A3 = X[,3];  A4 = X[,4];  A5 = X[,5]; 
A6 = X[,6]; A7 = X[,7]; A8 = X[,8];  A9 = X[,9];  A10 = X[,10]; 
Mfull = lm(y ~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10)
# install.packages('MuMIn')
library(MuMIn)
options(na.action = 'na.fail') # change the default 'na.omit' to prevent models
# from being fitted to different datasets in case of missing values.
Mdfull = dredge(Mfull)
MdfullAIC = dredge(Mfull,rank='AIC')
MdfullAIC
MdfullBIC = dredge(Mfull,rank='BIC')
MdfullBIC
}
# --------------------------------------------------------------------------
# Results & remarks: 
# - Correlation matrix C:
# > C
#       [,1]  [,2]  [,3]  [,4] [,5]  [,6]  [,7]  [,8]  [,9] [,10]
# [1,]  1.00  0.17  0.19  0.34 0.26  0.22 -0.08  0.20  0.27  0.30
# [2,]  0.17  1.00  0.09  0.24 0.04  0.14 -0.38  0.33  0.15  0.21
# [3,]  0.19  0.09  1.00  0.40 0.25  0.26 -0.37  0.41  0.45  0.39
# [4,]  0.34  0.24  0.40  1.00 0.24  0.19 -0.18  0.26  0.39  0.39
# [5,]  0.26  0.04  0.25  0.24 1.00  0.90  0.05  0.54  0.52  0.33
# [6,]  0.22  0.14  0.26  0.19 0.90  1.00 -0.20  0.66  0.32  0.29
# [7,] -0.08 -0.38 -0.37 -0.18 0.05 -0.20  1.00 -0.74 -0.40 -0.27
# [8,]  0.20  0.33  0.41  0.26 0.54  0.66 -0.74  1.00  0.62  0.42
# [9,]  0.27  0.15  0.45  0.39 0.52  0.32 -0.40  0.62  1.00  0.46
#[10,]  0.30  0.21  0.39  0.39 0.33  0.29 -0.27  0.42  0.46  1.00
# The magnitude of some of the correlations are quite high:
# C(5,6) = 0.90; C(7,8) = -0.74
# and thus there might be a co-linearity problem and there might be
# potential for reducing the number of predictors
# -----------
# - Multilinear regression:
# >   round(coef(summary(MLR1)),2)
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -357.43      67.06   -5.33     0.00
# X1             -0.04       0.22   -0.17     0.87
# X2            -22.86       5.84   -3.92     0.00
# X3              5.60       0.72    7.81     0.00
# X4              1.12       0.23    4.96     0.00
# X5             -1.09       0.57   -1.90     0.06
# X6              0.75       0.53    1.41     0.16
# X7              0.37       0.78    0.48     0.63
# X8              6.53       5.96    1.10     0.27
# X9            157.69      36.08    4.37     0.00
# X10             0.28       0.27    1.02     0.31
# According to the t-test, the estmated coefficients of X1 (age),
# X5 (tc), X6(), X7, X8, and X10 are not significantly different from
# zero on the level of evidence alpha = 0.05.
# Thus there is large potential for reducing the number of predictors.
# -> apply dredge() 
# ----
# - Pedestrian MLR
# >   print(c(round(t(betaMLR),2),'betaMLR (not standardized')) 
# -357.43 -0.04 -22.86  5.6 1.12 -1.09 0.75 0.37 6.53 157.69  0.28    
# Coefficients are identical to results using lm()
# ----
#  >   print(c(round(t(betaSDS),2),'betaSDS(lambda=0.1)'))
#  -0.47 -11.4 24.73 15.42 -36.75 21.93 4.39 8.31 35.38  3.22
# Coefficients based on standard deviation scaling (SDS) are different from
# values listed in Efron \& Hastie (2021, Table 7.3)
# ---
# >   print(c(round(t(betaULS),1),'betaULS(lambda=0.1)'))
# 1.31 -207.19 489.69 301.77 -83.47 -70.83 -188.68 115.71 443.81 86.75
# print(c(round(t(betaULS),1),'betaULS(lambda=0.1)'))
# 1.3 -207.2 489.7 301.8 -83.5 -70.8 -188.7 115.7 443.8  86.7
# Coefficients based on unit length scaling (ULS) are identical to
# values listed in Efron & Hastie (2021, Table 7.3)
# -----------------------------------------------------------------------
