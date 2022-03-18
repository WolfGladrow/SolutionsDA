print('file: Solution-MaleFemaleHeights.R')
# data: Zar (2010, p. 164)
# --------------------------------------------------------
MannWhitney = function(xA,yA) {                 # 1L
  # Mann-Whitney test
  # Dieter.Wolf-Gladrow@awi.de
  n = length(xA); m = length(yA); 
  xy = c(xA,yA); rxy = rank(xy); sry = sum(rxy[(n+1):(n+m)]);
  Usample = m*n+m*(m+1)/2-sry;
  # (a) large sample sizes -> normal approximation
  if ((n > 8) && (m > 8)) {                     # 2L
    UmeanAnalytic = n*m/2;
    sigmaUanalytic = sqrt(n*m*(n+m+1)/12);
    z = (Usample-UmeanAnalytic)/sigmaUanalytic; # standardized test statistic
    # z is approx. normally distributed for large nr -> approx. p-value
    pLeft = pnorm(z); pRight = 1-pnorm(z)}                                # 2R
  # (b) small sample sizes -> Monte Carlo estimate
  if ((n < 9) || (m < 9)) {                     # 3L
    jmin = sum(seq(1:m)); jmax = sum(seq(n+1,n+m));
    # -> U = m*n+m*(m+1)/2-sry can vary between
    Umin = m*n+m*(m+1)/2-jmax;
    Umax = m*n+m*(m+1)/2-jmin;
    fr = numeric(Umax+1);   # frequencies
    Urange = seq(0,Umax);   # for plot
    set.seed(1953) # set seed for random number generators
    M = 1e5;       # number of Monte Carlo runs
    Ua = numeric(M);
    for(j in 1:M) {                               # 4L
      x = rnorm(n); y = rnorm(m); # random samples from normal distribution
      z = c(x,y); rz = rank(z); 
      sry = sum(rz[(n+1):(n+m)]);
      U = m*n+m*(m+1)/2-sry;
      Ua[j] = U;
      fr[U+1] = fr[U+1] + 1
    }                                             # 4R
    Uamin = min(Ua);
    Uamax = max(Ua);
    pMC = fr/M;      # relative frequencies
    # probability for left & right tail:
    pLeft = sum(pMC[1:floor(Usample+1)]);  # pLeft
    pRight = sum(pMC[ceiling(Usample+1):(Umax+1)]);
  }                                               # 3R
  alpha = 0.05
  print(c('alpha = ',alpha))
  if (pLeft < alpha) print('xA < yA')
  if (pRight < alpha) print('xA > yA')
  if ((pLeft > alpha/2) && (pRight > alpha/2)) print('xA == yA')
  return(c(pLeft,pRight,Usample))
}     # 1R
# --------------------------------------------------------
xA = c(193,188,185,183,180,175,170) # (cm) height of males
yA = c(178,173,168,165,163)         # (cm) height of females
n = length(xA); m = length(yA)
alpha = 0.05; print(c(alpha,'alpha'))
pW = wilcox.test(xA,yA)$p.value; print(c(round(pW,4),'pW'))
if(pW < alpha) print(c('pW < alpha = ',alpha,': reject H0'))
if(pW >= alpha) print(c('pW >= alpha = ',alpha,': do not reject H0'))
# return(c(pLeft,pRight,Usample))
outMW = MannWhitney(xA,yA); 
pMW = outMW[2]; print(c(round(pMW,4),'pMW'))
if(pMW < alpha) print(c('pMW < alpha = ',alpha,': reject H0'))
if(pMW >= alpha) print(c('pMW >= alpha = ',alpha,': do not reject H0'))