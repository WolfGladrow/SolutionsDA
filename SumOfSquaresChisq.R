print('File: SumOfSquaresChisq.R')
# ----------------------------------------------------------------
# Exercise: Estimate the density of sum of squares divided by 
# sigma^2 using a Monte Carlo simulation -- sampling from a normal 
# distribution with variance sigma^2, sample size n=10, number of 
# Monte Carlo run M = 1e4 --and compare it with the chi^2 density 
# for df = n degrees of freedom.
# Dieter.Wolf-Gladrow@awi.de 10/2024
# ----------------------------------------------------------------
set.seed(1953) # set seed for random number generators
# sum of squares scale with sigma^2
n = 10; M = 1e4; sigma = 2
SS = numeric(M); 
for(j in 1:M) SS[j] = sum((rnorm(n,0,sigma))^2)
xp = seq(0,3*n,0.01); yp = dchisq(xp,df=10)
# png('SumOfSquaresChisq241004c.png',width=16,height=16,units='cm',res=300)
plot(density(SS/sigma^2,from=0,to=3*n),type='l',lwd=4,
       col='blue',xlab='x',ylab='y',las=1,cex=0.6,
       cex.lab=1.5,main='')
lines(xp,yp,col='magenta',lwd=4,lty=4)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
#   Sum of squares / sigma^2 follows chisq-pdf with nu=n
# ----------------------------------------------------------------