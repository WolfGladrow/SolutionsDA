print('file: SolutionFisher25Poisson.R')
# EXERCISE & SOLUTION: Fisher (1925, Table 14) Poisson
# Remark: The expected frequencies given by Fisher in Table 14 are derived from
# the Poisson distribution with $\lambda = 4.68$ (the value estimated from the 
# original values given in Table 5) and the total number 400. The first frequency 
# value, 21.08, is the sum of the values 3.71 and 17.37 for 0 and 1, respectively. 
# The last value, 8.66, is the sum of all values from 10 to infinity. 
# For details of the calculation see the R code:
# Original data: Table 5
xp0 = seq(0,12)                             # original data 
fobs0=c(0,20,43,53,86,70,54,37,18,10,5,2,2) # original data (Table 5)
total0 = sum(fobs0)
print('Lumped data: Table 14')
lambdaEst0 = sum(fobs0*xp0)/total0   # estimate lambda
xp = seq(1,10)                       # lumped (Table 14)
fobs=c(20,43,53,86,70,54,37,18,10,9) # lumped (Table 14)
total = sum(fobs)
print('Expected frequencies')
fexp0 = dpois(xp0,lambdaEst0)*total
print('Lump expected frequencies')
fexp1 = numeric(10)
fexp1[1] = fexp0[1] + fexp0[2]  # lump 0 and 1
fexp1[2:9] = fexp0[3:10]
fexp1[10] = total0-sum(fexp1[1:9]) # lump right tail
chisq1 = sum((fobs-fexp1)^2/fexp1)
nu = length(fexp1) - 2 # 2 constraints: total & lambda
p = (1-pchisq(q=chisq1,df=nu))
# png('Fisher25Poisson190415.png',width=16,height=16,units='cm',res=300)
plot(xp0,fobs0,type='p',lwd=4,col='blue',xlab='x',ylab='Frequency',las=1,cex=0.6,
     ylim=c(0,100),cex.lab=1.5)
points(xp0,fexp0,col='magenta',lwd=4,pch=24,cex=0.6)
# dev.off()