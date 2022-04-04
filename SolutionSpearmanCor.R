print('file: SolutionSpearmanCor.R')
# EXERCISE & SOLUTION: Spearman correlation matrix II
# (1) generate data:
n = 10  # sample size
set.seed(1953)   # set seed for random number generators
x1 = seq(0,90,10)
x2 = 30+0.5*x1+5*rnorm(n)       # cov(x1,x2) > 0
x3 = 40-0.6*x1+8*rnorm(n)       # cov(x1,x3) < 0
x4 = 30*rnorm(n)-10*rf(n,15,3)  # cov(x1,x4) approx. 0
# round to 1 digit after dot:
x1r = round(x1,1); x2r = round(x2,1); x3r = round(x3,1); x4r = round(x4,1); 
X = matrix(data=c(x1r,x2r,x3r,x4r),nrow=n,ncol=4)
# (2) calculate Pearson & Spearman correlation matrices:
c = cor(X)   # Pearson
cSp = cor(X,method='spearman')   # Spearman
# The pedestrian way:
cSpPedestrian = matrix(data=NA,nrow=4,ncol=4)
for(i in 1:4) for(j in 1:4) cSpPedestrian[i,j]=cor(rank(X[,i]),rank(X[,j]))
# (3) results:
round(c,3)
#        [,1]   [,2]   [,3]   [,4]
# [1,]  1.000  0.964 -0.828 -0.125
# [2,]  0.964  1.000 -0.779 -0.192
# [3,] -0.828 -0.779  1.000  0.085
# [4,] -0.125 -0.192  0.085  1.000
round(cSp,3)
#        [,1]   [,2]   [,3]   [,4]
# [1,]  1.000  0.952 -0.733 -0.333
# [2,]  0.952  1.000 -0.685 -0.406
# [3,] -0.733 -0.685  1.000  0.224
# [4,] -0.333 -0.406  0.224  1.000
cSpPedestrian       # is identical to cSp
cSpPedestrian - cSp # test: order(1e-16)
# --------------------------------------- Kendall:
cK = cor(X,method='kendall')   # Kendall
round(cK,3)
#        [,1]   [,2]   [,3]   [,4]
# [1,]  1.000  0.867 -0.600 -0.289
# [2,]  0.867  1.000 -0.556 -0.333
# [3,] -0.600 -0.556  1.000  0.156
# [4,] -0.289 -0.333  0.156  1.000