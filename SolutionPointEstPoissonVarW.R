print('file: SolutionPointEstPoissonVarW.R')
# EXERCISE & SOLUTION: Poisson lambda_est = a*mean+(1-a)*var
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
lambda1 = 2.7  # rate (mean number of events) = variance
muest = numeric(M); varest = numeric(M)
for(m in 1:M) {r = rpois(n,lambda=lambda1);
muest[m]=mean(r); varest[m]=var(r)}
a = seq(0,2,0.01); L = length(a)
VarW = numeric(L); Wa = numeric(M)
for(k in 1:L) {Wa=a[k]*muest+(1-a[k])*varest; VarW[k] = var(Wa)}
aopt = a[which.min(VarW)]
# png('PoissonWa161224.png',width=16,height=16,units='cm',res=300)
plot(a,VarW,type='l',lwd=4,col='blue',xlab='a',ylab='VarW',las=1,cex.lab=1.5)
# dev.off()
