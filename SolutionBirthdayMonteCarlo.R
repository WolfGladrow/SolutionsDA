print('file: SolutionBirthdayMonteCarlo.R')
# probability for people having birthday at same day
set.seed(1953) # set seed for random number generators
M = 1e5        # number of Monte Carlo runs
f2 = 0; f4 = 0; f6 = 0; f8 = 0; fg8 = 0; fg1 = 0 # frequencies
n = 26 # number of students, Marine Biology Course in Bremen 2019
for(k in 1:M) {
  xrange = seq(1,365)
  x = sample(x=xrange,n,replace=TRUE) # random sample from uniform PD
  A = matrix(data=NA,nrow=n,ncol=n)   # matrix containing differences between birthdays
  for(i in 1:n) for(j in 1:n) A[i,j] = x[i]-x[j]
  q = sum(A==0)-n  # -n because n zeros on diagonal of A
  if(q == 2) f2=f2+1 # birthday of two persons on same date
  if(q == 4) f4=f4+1 # 2 x 2 or 1 x 4
  if(q == 6) f6=f6+1 # 3 x 2 or 2 x 3 or 1 x 6
  if(q == 8) f8=f8+1
  if(q > 8) fg8 = fg8+1
  if(q > 1) fg1 = fg1+1
}
print(c(n,'n'))
print(c(M,'M'))
p2 = f2/M; print(c(p2,'p2'))
p4 = f4/M; print(c(p4,'p4'))
p6 = f6/M; print(c(p6,'p6'))
p8 = f8/M; print(c(p8,'p8'))
pg8 = fg8/M; print(c(pg8,'pg8'))
pg1 = fg1/M; print(c(pg1,'pg1'))  # all in one
print(c(p2+p4+p6+p8+pg8,'p2+p4+p6+p8+pg8'))
print('Probability for at least 2 have birthday on the same date:')
pnbN = 1;
for(k in 2:n) pnbN = pnbN*(366-k)/365; # probability for no joint birthday
pb = 1-pnbN  # probability for joint birthday
print(c(round(pb,4),'pb'))