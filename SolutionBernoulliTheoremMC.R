print('file: BernoulliTheoremMC.R')
# Bernoulli theorem & Monte Carlo
# Estimation:
# n -> x_1, ..., x_n -> pEst = mean(x) +- mean(x)/est(sigma)
# (<-> p +- dp)
# -> How large is Pr such that p within mean(x) +- mean(x)/est(sigma)
set.seed(1953) # set seed for random number generators
p = 1/6 # (true) probability for success in single trial for fair (unbiased) die
n = 1e4 # sample size
SEana = sqrt(p*(1-p))/sqrt(n)
x = rbinom(n,1,p)
pEst = mean(x)
upEst = sd(x)/sqrt(n) # standard error of the mean (estimated)
print(c(sum(x),'sum(x)'))
print(c(round(p,5),'p'))
print(c(round(pEst,5),'pEst = mean(x)'))
print(c(round(SEana,5),'standard error SEana'))
print(c(round(upEst,5),'upEst'))
print(' ---------------------------------------------------')
print(' Apply Theorem of Bernoulli:')
a = sum(x); pEst = a/n; 
sr = sqrt(2*a*(n-a)/n^3)
dp = upEst
Pr1 = pEst-dp; Pr2 = pEst+dp; q = dp/sr
integrand <- function(x) {exp(-x^2)}
out = integrate(integrand, lower = 0, upper = q)
Pr = out$value*2/sqrt(pi)
print(c(n,'n')); print(c(a,'a')); print(c(pEst,'pEst'))
print(c(sr,'sr')); print(c(dp,'dp'))
print(c(q,'q')); print(c(Pr,'Pr'))
print(c(Pr1,'Pr1')); print(c(Pr2,'Pr2'))