print('file: SolutionCollComp.R')
# EXERSICE & SOLUTION: collinearity, compensation, acetylene data
# -------------------------------------------
# (1) = observed data (Yo,X1o,X2o,X3o) and quadratic predictors
Yo = c(49.0,50.2,50.5,48.5,47.5,44.5,28.0,31.5,34.5,35.0,38.0,38.5,15.0,17.0,20.5,29.5)
X1o = c(1300,1300,1300,1300,1300,1300,1200,1200,1200,1200,1200,1200,1100,1100,1100,1100)
X2o = c(7.5,9.0,11.0,13.5,17.0,23.0,5.3,7.5,11.0,13.5,17.0,23.0,5.3,7.5,11.0,17.0)
X3o = c(12,12,11.5,13,13.5,12,40,38,32,26,34,41,84,98,92,86)/1000
X4o = X1o*X2o; X5o = X1o*X3o; X6o = X2o*X3o
X7o = X1o^2; X8o = X2o^2; X9o = X3o^2
LYo = length(Yo)   # sample size
NoP = 9            # number of predictors
# -------------------------------------------
# (2) Unit length scaling:
Xo = matrix(data=c(X1o,X2o,X3o,X4o,X5o,X6o,X7o,X8o,X9o),nrow=LYo,ncol=NoP)
# Unit length scaling = subtract mean (centering) and divide by square root of sum of squares
X = matrix(data=NA,nrow=LYo,ncol=NoP)
n = NoP
meanX = numeric(n); SX = numeric(n)
for(j in 1:NoP) {q = Xo[,j]; xj = (q-mean(q))/sqrt(sum((q-mean(q))^2)); 
X[,j] = xj; meanX[j] = mean(q); SX[j] = sqrt(sum( (q-mean(q))^2 ))}
meanY = mean(Yo)
SY = sqrt(sum((Yo-meanY)^2))
y = (Yo-meanY)/SY
# -------------------------------------------
# (3a) MLR of original data using lm():
outLMo = lm(Yo ~ Xo)
InterceptEsto = outLMo$coefficients[1]
bEsto = outLMo$coefficients[2:(NoP+1)]
# (3b) MLR of scaled data using lm():
outLMs = lm(y ~ X)
betaEst1 = outLMs$coefficients[2:(NoP+1)]
# -------------------------------------------
print('Compensation mechanism:')
v1 = betaEst1[1]*X[,1]
v3 = betaEst1[3]*X[,3]
v5 = betaEst1[5]*X[,5]
v7 = betaEst1[7]*X[,7]
v1v3 = v1+v3
v1v7 = v1+v7
v3v5 = v3+v5
v5v7 = v5+v7
v1v3v5v7 = v1v3+v5v7
print(c('v1 = ',round(v1,1)))
print(c('v3 = ',round(v3,1)))
print(c('v5 = ',round(v5,1)))
print(c('v7 = ',round(v7,1)))
print(c('v1v3 = ',round(v1v3,1)))
print(c('v1v7 = ',round(v1v7,1)))
print(c('v3v5 = ',round(v3v5,1)))
print(c('v5v7 = ',round(v5v7,1)))
print(c('v1v3v5v7 = ',round(v1v3v5v7,1)))
# -----------------------------------------------------------------------
print(c('Lv1 = ',round(sqrt(sum(v1^2)),1)))
print(c('Lv3 = ',round(sqrt(sum(v3^2)),1)))
print(c('Lv5 = ',round(sqrt(sum(v5^2)),1)))
print(c('Lv7 = ',round(sqrt(sum(v7^2)),1)))
print(c('Lv1v3 = ',round(sqrt(sum(v1v3^2)),1)))
print(c('Lv1v7 = ',round(sqrt(sum(v1v7^2)),1)))
print(c('Lv3v5 = ',round(sqrt(sum(v3v5^2)),1)))
print(c('Lv5v7 = ',round(sqrt(sum(v5v7^2)),1)))
print(c('Lv1v3v5v7 = ',round(sqrt(sum(v1v3v5v7^2)),1)))