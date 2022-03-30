print('file: SolutionNormalRNlinCom.R')
# linear combination: sum of random numbers from two normal distributions: 3.2*X + 2.7*Y
#     My guess: mu_diff = 3.2*mu1-2.7*mu2; sigma^2_diff = 3.2^2*sigma1^2 + 2.7^2*sigma2^2
#     Method: Monte Carlo
set.seed(1953) # set seed for random number generators
M = 1e5   # number of Monte Carlo runs
mu1 = 2; mu2 = 1.5; sigma1 = 2; sigma2 = 1;
c1 = 3.2; c2 = 2.7
d = c1*rnorm(M,mu1,sigma1) - c2*rnorm(M,mu2,sigma2)
# png('NormalLinCombi160107.png',width=16,height=16,units='cm',res=300)
hist(d,300,col='blue',xlab='Linear combination',main='',las=1,cex.lab=1.5)
# dev.off()
# ---------------------------------------------------------------------------
mudguess = c1*mu1-c2*mu2; print(c(mudguess,'mean difference (guess)'))    
mudEst = mean(d); print(c(round(mudEst,4),'mean difference (estimated)')) 
vardguess = c1^2*sigma1^2+c2^2*sigma2^2; 
print(c(vardguess,'variance of difference (guess)'))
vardEst = var(d); print(c(round(vardEst,4),'variance of difference (estimated)'))