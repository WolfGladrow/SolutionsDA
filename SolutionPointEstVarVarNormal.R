print('file: SolutionPointEstVarVarNormal.R')
#   Estimate uncertainty of sigma^2 from random sample of normal distribution
#     variance sigma^2 = 4 EXERCISE (2/2016)
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
varest = numeric(M)
for (m in 1:M) varest[m]=var(rnorm(n,mean=0,sd=2))
# png('MCvarNormalExercise160227.png',width=16,height=16,units='cm',res=300)
hist(varest,30,col='blue',xlab=NA,main='',las=1,cex.lab=1.5)
title(xlab=TeX('$\\hat{\\sigma^2}_a = \\frac{1}{n-1} \\sum_j (x_j - \\hat{\\mu})^2$'),line=4.3,cex.lab=1.5)
meanvarest = mean(varest); print(c('meanvarest = ',meanvarest))  # (expected: close to sigma^2 = 4)
varvarest = var(varest); print(c('varvarest = ',varvarest))     # (no expectation)
sdvarest = sd(varest); print(c('sdvarest = ',sdvarest))        # 2.989269
print(c('varEst2/varEst1 = ',varvarest/0.5584831))  # 16.0000012818457
meanvarestr = round(meanvarest,3); varvarestr = round(varvarest,3); sdvarestr = round(sdvarest,3)
xt = 12
text(xt,150,bquote(~mean(hat(sigma^2)[a]) == .(meanvarestr)),col='blue',cex=1.5)
text(xt,120,bquote(~var(hat(sigma^2)[a]) == .(varvarestr)),col='blue',cex=1.5)
text(xt,90,bquote(~sd(hat(sigma^2)[a]) == .(sdvarestr)),col='blue',cex=1.5)
abline(v=4,col='black',lty=1)
abline(v=meanvarest,col='blue',lty=4)
# dev.off()