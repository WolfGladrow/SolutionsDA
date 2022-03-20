print('file: SolutionAcetyleneCorMat.R')
# EXERCISE & SOLUTION: correlation matrix of original acetylene data
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
Xall = c(X1o,X2o,X3o)
MXall = matrix(data=Xall,ncol=3)
# -----------------------------------------
CM = matrix(data=NA,nrow=3,ncol=3)
for(k in 1:3) CM[k,k] = 1
CM[1,2] = cor(X1o,X2o); CM[2,1] = CM[1,2]
CM[1,3] = cor(X1o,X3o); CM[3,1] = CM[1,3]
CM[3,2] = cor(X3o,X2o); CM[2,3] = CM[3,2]
# ----------------------------------------
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
Xall9 = c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o)
MXall9 = matrix(data=Xall9,ncol=9)
cm = cor(MXall9)
source('plotCorMatrix.R')
# png('AcetyleneCM201010.png',width=16,height=12,units='cm',res=300)
plotCorMatrix(cm)
# dev.off()