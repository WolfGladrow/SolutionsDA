print('file: SolutionBirthdayNonUniform2607.R')
print(date())
# Probability for people having birthday at same day: random sampling
#   from a non-uniform probability distribution
# Data source:
# https://www.panix.com/~murphy/bdata.txt
# ------------------------------------------------------------------
print('--------------------------------------------------')
print(' (1) Load data and retrive frequencies (f)')
print('--------------------------------------------------')
Q = read.table(file='Birthday365.txt',header=FALSE)
f = Q$V2 # frequencies (number of births per day) for days 1 to 365
L = length(f) # 365
print('--------------------------------------------------')
print(' (1a) Plot frequency data:')
print('--------------------------------------------------')
sflag = 0
if (sflag == 1) {
# png('BirthdayObservedFrequencies260729.png',width=16,height=16,units='cm',res=300)
plot(f,type='p',lwd=1,col='blue',xlab='Day',
       ylab='Frequencies',las=1,cex=0.6,cex.lab=1.5,
       ylim=c(0,max(f)))
(fmean = mean(f)) # 1317.027
(iMax = which.max(f)) # 227 
(iMax/30)             # 7.566667
(iMax/30-9)           # -1.4
yp = c(0,1000)
for (i in 1:11) {
      xp=c(i,i)*30
      lines(xp,yp,col='black')
}
abline(h=fmean,col='magenta')
xp1=c(iMax,iMax); yp1 = c(0,1100)
lines(xp1,yp1,col='green')
# dev.off()
}
print('--------------------------------------------------')
print(' (2) Estimate probability distribution (PDb) from frequencies')
print('--------------------------------------------------')
s = sum(f) # sum of frequencies 480715
PDb = f/s  # estimate of PD from frequencies by normalization
print('--------------------------------------------------')
print(' (2a) Plot estimated PD:')
print('--------------------------------------------------')
sflag = 0
if (sflag == 2) {
    # png('BirthdayPD260729.png',width=16,height=16,units='cm',res=300)
    plot(PDb*1000,type='p',lwd=1,col='blue',xlab='Day',
         ylab='Probabilities*1000',las=1,cex=0.6,cex.lab=1.5,
         ylim=c(0,max(PDb*1000)))
    (PDbmean = mean(PDb*1000)) # 2.739726
    (iMax = which.max(PDb)) # 227 
    (iMax/30)             # 7.566667
    (iMax/30-9)           # -1.4
    yp = c(0,2)
    for (i in 1:11) {
      xp=c(i,i)*30
      lines(xp,yp,col='black')
    }
    abline(h=PDbmean,col='magenta')
    xp1=c(iMax,iMax); yp1 = c(0,2.2)
    lines(xp1,yp1,col='green')
    # dev.off()
}
print('--------------------------------------------------')
print(' (3) Estimate cumulative density function (CDF): step function')
print('--------------------------------------------------')
CDF = numeric(L) # allocate memory space
CDF[1] = PDb[1]
for(k in 2:L) CDF[k] = CDF[k-1]+PDb[k]
(CDF[L])   # check: should be 1   1
(1-CDF[L]) # check: should be 0   1.221245e-15 is o.k.
print('--------------------------------------------------')
print(' (4) Function for random sampling from CDF:')
print('--------------------------------------------------')
set.seed(1953) # set seed for random number generators
BirthdaySample = function(n) {
    # Random sample of length n from given CDF
    jSample = numeric(n)
    for (k in 1:n) {
      r = runif(1) # random number between 0 and 1
      # Find index jMin for which CDR[jMin] is closest to r:
      jMin = which.min((CDF-r)^2) 
      # The searched for day j can be equal to jMin or equal to jMin+1:
      if (jMin == 1) { # min(CDF) = 0
        x1 = 0; x2 = CDF[1]; x3 = CDF[2]
        if ((x1 <= r) && (r <= x2)) j=1
        if ((x2 <= r) && (r <= x3)) j=2
      }
      if ((jMin > 1) && (jMin < L)) {
        x1 = CDF[jMin-1]; x2 = CDF[jMin]; x3 = CDF[jMin+1]
        if ((x1 <= r) && (r <= x2)) j=jMin
        if ((x2 <= r) && (r <= x3)) j=jMin+1
      }
      if (jMin == L) j = jMin # max(CDF) = 1
      jSample[k] = j
    }
    return(jSample)
}
print('--------------------------------------------------')
print(' (4a) Test random sampling:')
print('--------------------------------------------------')
# Sample size same as original data size s:
xTest = BirthdaySample(s) # random sampling from CDF
print('--------------------------------------------------')
print(' (4b) Plot histogram of data:')
print('--------------------------------------------------')
sflag = 3
if (sflag == 3) {
    # png('BirthdayNonuniform260729.png',width=16,height=16,units='cm',res=300)
    mybreaks = seq(0.5,L+0.5,1)
    h = hist(xTest,breaks=mybreaks,col='blue',las=1,main='',xlab='Day') 
    q = h$counts
    (j = which.max(q)) # 265
    (m = j/30) # 8.833333 September
    (xTestmean = mean(h$counts)) # 1317.027
    abline(h=xTestmean,col='magenta')
    # dev.off()
}
print('--------------------------------------------------')
print(' (5) Monte Carlo simulation:')
print('--------------------------------------------------')
M = 1e5        # number of Monte Carlo runs
f2 = 0; f4 = 0; f6 = 0; f8 = 0; fg8 = 0; fg1 = 0 # frequencies
n = 26 # number of students, Marine Biology Course in Bremen 2019
for(k in 1:M) {
    xrange = seq(1,L)
    # x = sample(x=xrange,n,replace=TRUE) # random sample from uniform PD
    x = BirthdaySample(n) # random sampling from CDF
    A = matrix(data=NA,nrow=n,ncol=n)   # matrix containing differences between birthdays
    for(i in 1:n) for(j in 1:n) A[i,j] = x[i]-x[j]
    q = sum(A==0)-n  # -n because n zeros on diagonal of A
    if(q == 2) f2=f2+1 # 1 x 2 = birthday of two persons on same date 
    if(q == 4) f4=f4+1 # 2 x 2 or 1 x 4
    if(q == 6) f6=f6+1 # 3 x 2 or 2 x 3 or 1 x 6
    if(q == 8) f8=f8+1 # 4 x 2 or (2 x 3 + 1 x 2) or (2 x 2 + 2 x 2) or 2 x 4 or 1 x 8
    if(q > 8) fg8 = fg8+1
    if(q > 1) fg1 = fg1+1
}
print(c(n,'n'))
print(c(M,'M'))
p2 = f2/M; print(c(p2,'p2: 1 x 2: prob(2 birthdays on same day)'))
p4 = f4/M; print(c(p4,'p4: 2 x 2 or 1 x 4'))
p6 = f6/M; print(c(p6,'p6: 3 x 2 or 2 x 3'))
p8 = f8/M; print(c(p8,'p8: 4 x 2 or ...'))
pg8 = fg8/M; print(c(pg8,'pg8'))
pg1 = fg1/M; print(c(pg1,'pg1'))  # all in one
print('Probability for at least 2 have birthday on the same date')
print('Monte Carlo simulation:')
print(c(p2+p4+p6+p8+pg8,'p2+p4+p6+p8+pg8'))
print('--------------------------------------------------')
print(' (6) Analytical solution for at least 2 have birthday on the same date')
print('--------------------------------------------------')
pnbN = 1;
for(k in 2:n) pnbN = pnbN*(366-k)/365; # probability for no joint birthday
pb = 1-pnbN  # probability for joint birthday
print('Probability for at least 2 have birthday on the same date')
print('analytic solution:')
print(c(round(pb,5),'pb'))
(d = (pg1-pb)/(pg1+pb)*200) # (%) 0.68 
# -----------------------------------------------------------------
# [1] "file: R_BirthdayNonUniform2607.R"
# [1] "history: SolutionBirthdayMonteCarlo.R"
# [1] "Wed Jul 29 22:48:34 2026"
# [1] "12 = non-uniform PD of birthdays: clean version (7/2026)"
# [1] "--------------------------------------------------"
# [1] " (1) Load data and retrive frequencies (f)"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (1a) Plot frequency data:"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (2) Estimate probability distribution (PDb) from frequencies"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (2a) Plot estimated PD:"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (3) Estimate cumulative density function (CDF): step function"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (4) Function for random sampling from CDF:"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (4a) Test random sampling:"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (4b) Plot histogram of data:"
# [1] "--------------------------------------------------"
# [1] "--------------------------------------------------"
# [1] " (5) Monte Carlo simulation:"
# [1] "--------------------------------------------------"
# [1] "26" "n" 
# [1] "1e+05" "M"    
# [1] "0.3864"                                  
# [2] "p2: 1 x 2: prob(2 birthdays on same day)"
# [1] "0.15618"            "p4: 2 x 2 or 1 x 4"
# [1] "0.04431"            "p6: 3 x 2 or 2 x 3"
# [1] "0.01171"          "p8: 4 x 2 or ..."
# [1] "0.00375" "pg8"    
# [1] "0.60235" "pg1"    
# [1] "Probability for at least 2 have birthday on the same date"
# [1] "Monte Carlo simulation:"
# [1] "0.60235"         "p2+p4+p6+p8+pg8"
# [1] "--------------------------------------------------"
# [1] " (6) Analytical solution for at least 2 have birthday on the same date"
# [1] "--------------------------------------------------"
# [1] "Probability for at least 2 have birthday on the same date"
# [1] "analytic solution:"
# [1] "0.59824" "pb"    
# -----------------------------------------------------------------