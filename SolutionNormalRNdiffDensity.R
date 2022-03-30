print('file: SolutionNormalRNdiffDensity.R')
# difference of two samples from normal distributions follow a normal distribution
#     My guess: mu_diff = mu1-mu2; sigma^2_diff = sigma1^2 + sigma2^2
#     Method: Monte Carlo
M = 1e5   # number of Monte Carlo runs
mu1 = 2; mu2 = 1; sigma1 = 2; sigma2 = 3;
set.seed(1953) # set seed for random number generators
d = rnorm(M,mu1,sigma1) - rnorm(M,mu2,sigma2)
# png('diffNormalDens160818.png',width=16,height=16,units='cm',res=300)
plot(density(d,from=-11,to=11),type='l',lwd=3,col='blue',main='',
       xlab='Difference X - Y',ylab='Density estimate',las=1,xlim=c(-10,10),cex.lab=1.5)
# dev.off()
# ---------------------------------------------------------------------------
mudguess = mu1-mu2; print(c(mudguess,'mean difference (guess)'))          # 1
mudEst = mean(d); print(c(round(mudEst,4),'mean difference (estimated)')) # 0.9988381 for M=100000
vardguess = sigma1^2+sigma2^2; print(c(vardguess,'variance of difference (guess)'))   # 13 
vardEst = var(d); print(c(round(vardEst,4),'variance of difference (estimated)'))     # 13.0487