print('file: SolutionSLRBayesian.R')
print(date())
# purpose: 
# created by: Dieter.Wolf-Gladrow@awi.de 
#    8/2024 version 1.0
# ---------------------------------------------------------
print('Simple linear regression: Bayesian approach (8/2024)')
print('based on Zellner (1971)')
# generate & plot artificial data & mean values (with respect to y)
  beta1 = 8; beta2 = -1.2 # true intercept & slope
  sigma = 1               # true standard deviation of normal noise
# ----------- generate data (begin)
# on purpose: small sample size in order to recognize differences
#    of marginal posterior distributions (for intercepts, slope,
#    noise level) from normal distributions.
  set.seed(1953)                    # set seed for random number generators
  x = seq(0.5,5,0.5); n = length(x) # non-stochastic (uncertainty = 0)
  y = beta1+beta2*x+sigma*rnorm(n)  # additive normal noise
  print(c(n,'sample size n'))
  # ----------- generate data (end)
  # ----------- prepare data (matrix -> array) & apply lm()
  out1 = lm(y ~ x)   # use tilde sign to relate response (y) 
  #    to predictor (x)
  out2 = summary(out1)     # summary gives more information, especially the uncertainties
  b1 = out2$coefficients[1];  print(c(round(b1,4),'intercept b1 (estimate)'))
  b2 = out2$coefficients[2];   print(c(round(b2,4),'slope b2 (estimate)'))
  ub1 = out2$coefficients[3]; print(c(round(ub1,4),'intercept uncertainty ub1 (estimate)'))
  ub2 = out2$coefficients[4];  print(c(round(ub2,4),'slope uncertainty ub2 (estimate)'))
  # rounding (for text in plot):
  b1r = round(b1,2); b2r = round(b2,2); 
  ub1r = round(ub2,2); ub2r = round(ub2,2); 
  # --------------------------- plot:
  sflag = 0
  if (sflag == 1) {
    # png('SLRsmallEstimates240831.png',width=16,height=16,units='cm',res=300)
    plot(x,y,type='p',lwd=4,col='black',cex=0.6,ylim=c(0,10),
         xlim=c(0,max(x)),xlab='x',ylab='y',las=1,cex.lab=1.5)
    xp = c(min(x),max(x))
    yp = out1$coefficients[1]+out1$coefficients[2]*xp
    lines(xp,yp,col='magenta',lwd=3)
    xt = 0.1   # x-position for text
    text(xt,2,bquote(~hat(beta)[1] == .(b1r) %+-% .(ub1r)),col='magenta',cex=1.5,pos=4)
    text(xt,0.5,bquote(~hat(beta)[2] == .(b2r) %+-% .(ub2r)),col='magenta',cex=1.5,pos=4)
    xt = 3
    text(xt,9,bquote(~beta[1] == .(beta1)),col='black',cex=1.5,pos=4)
    text(xt,7.5,bquote(~beta[2] == .(beta2)),col='black',cex=1.5,pos=4)
    # dev.off()
  }
  sigmaEst = out2$sigma 
  print(c(round(sigmaEst,4),'sigmaEst')) # 0.8931
  print(' ---------------------------------------------------')
  print('Marginal posterior pdfs for intercept: Eq. 3.11')
  dx = 0.001
  beta1Arr = seq(5,11,dx)
  nu = out2$df[2]; print(c(nu,'degrees of freedom nu'))
  xmean = mean(x)
  p1 = (nu+sum((x-xmean)^2)/(sigmaEst^2*sum(x^2)/n)*
          (beta1Arr-b1)^2)^(-(nu+1)/2)
  qnorm = sum(p1)*dx
  p1 = p1/qnorm # normalize to 1
  sflag = 0
  if (sflag == 2) {
    # png('SLRmarginalPostInterceptS240831.png',width=16,height=12,units='cm',res=300)
    plot(beta1Arr,p1,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,
         las=1,cex=0.4)
    title(xlab=TeX('Intercept $\\beta_1$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\beta_1$'),line=2.5)
    # dev.off()
  }
  print(' ---------------------------------------------------')
  print('Marginal posterior pdfs for slope: Eq. 3.12')
  beta2Arr = seq(-2,0,dx)
  p2 = (nu+sum((x-xmean)^2)/sigmaEst^2*
          (beta2Arr-b2)^2)^(-(nu+1)/2)
  qnorm2 = sum(p2)*dx
  p2 = p2/qnorm2 # normalize to 1
  sflag = 0
  if (sflag == 3) {
    # png('SLRmarginalPostSlopeS240831.png',width=16,height=12,units='cm',res=300)
    plot(beta2Arr,p2,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,
         las=1,cex=0.4)
    title(xlab=TeX('Slope $\\beta_2$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\beta_2$'),line=2.5)
    # dev.off()
  }
  print(' ---------------------------------------------------')
  print('Marginal posterior for sigma: Eq. (A.37b)')
  sr = round(sigmaEst,4) # estimate of sigma (from flag == 52)
  sigma = seq(0.4,2,0.001)
  p = 2/gamma(nu/2)*(nu*sigmaEst^2/2)^(nu/2)*
    exp(-nu*sigmaEst^2/(2*sigma^2))/sigma^(nu+1)
  sflag = 0
  if (sflag == 4) {
    # png('SLRmZellner71EqAd37bx2408.png',width=16,height=12,units='cm',res=300)
    plot(sigma,p,type='l',lwd=3,col='blue',xlab=NA,
         ylab=NA,las=1,cex=0.4)
    title(xlab=TeX('$\\sigma$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\sigma$'),line=2.5)
    text(0.7,0.6,bquote(~nu == .(nu)),col='blue',cex=1.5,pos=4)
    text(0.7,0.1,bquote(~s == .(sr)),col='black',cex=1.5,pos=4)
    # dev.off()
  }
  print(' ---------------------------------------------------')
  print('Exercise: (1) Calculate means and standard deviation for')
  print('the marginal posterior distributions of intercept, slope,')
  print('and sigma. (2) Add the normal approximations to the')
  print('plots given above.')
  print(' ---------------------------------------------------')
  print('Intercept PDF: normal approximation')
  p1mean = sum(beta1Arr*p1)*dx; print(c(round(p1mean,4),'p1mean'))
  p1var = sum((beta1Arr-p1mean)^2*p1)*dx; 
  print(c(round(p1var,4),'p1var'))
  p1sd = sqrt(p1var)
  print(c(round(p1sd,4),'p1sd'))
  muN = round(p1mean,2); sigmaN = round(p1sd,2) # -> plot
  sflag = 0
  if (sflag == 5) { # intercept PDF & normal approximation
    xp = seq(5,11,dx)
    yp = dnorm(xp,p1mean,p1sd)
    # png('SLRmarginalPostInterceptS240831NA.png',width=16,height=12,units='cm',res=300)
    plot(beta1Arr,p1,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,
         las=1,cex=0.4)
    lines(xp,yp,col='magenta',lty=4)
    title(xlab=TeX('Intercept $\\beta_1$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\beta_1$'),line=2.5)
    text(5,0.5,bquote(~mu[N] == .(muN)),col='magenta',cex=1.5,pos=4)
    text(5,0.4,bquote(~sigma[N] == .(sigmaN)),col='magenta',cex=1.5,pos=4)
    # dev.off()
  }
  print(' ---------------------------------------------------')
  print('Slope PDF: normal approximation')
  p2mean = sum(beta2Arr*p2)*dx; print(c(round(p2mean,4),'p2mean'))
  p2var = sum((beta2Arr-p2mean)^2*p2)*dx; 
  print(c(round(p2var,4),'p2var'))
  p2sd = sqrt(p2var)
  print(c(round(p2sd,4),'p2sd'))
  mu2N = round(p2mean,2); sigma2N = round(p2sd,2) # -> plot
  sflag = 0
  if (sflag == 6) { # slope PDF & normal approximation
    xp = seq(-2,0,dx)
    yp = dnorm(xp,p2mean,p2sd)
    # png('SLRmarginalPostSlopeS240831NA.png',width=16,height=12,units='cm',res=300)
    plot(beta2Arr,p2,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,
         las=1,cex=0.4)
    lines(xp,yp,col='magenta',lty=4)
    title(xlab=TeX('Slope $\\beta_2$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\beta_2$'),line=2.5)
    text(-0.8,1.5,bquote(~mu[N] == .(mu2N)),col='magenta',cex=1.5,pos=4)
    text(-0.8,1.2,bquote(~sigma[N] == .(sigma2N)),col='magenta',cex=1.5,pos=4)
    # dev.off()
  }
  print(' ---------------------------------------------------')
  print('sigma PDF: normal approximation')
  pSmean = sum(sigma*p)*dx; print(c(round(pSmean,4),'pSmean'))
  pSvar = sum((sigma-pSmean)^2*p)*dx; 
  print(c(round(pSvar,4),'pSvar'))
  pSsd = sqrt(pSvar)
  print(c(round(pSsd,4),'pSsd'))
  muSN = round(pSmean,2); sigmaSN = round(pSsd,2) # -> plot
  sflag = 7
  if (sflag == 7) {
    xp = seq(0.4,1.6,dx)
    yp = dnorm(xp,pSmean,pSsd)
    # png('SLRmZellner71EqAd37bx2408NA.png',width=16,height=12,units='cm',res=300)
    plot(sigma,p,type='l',lwd=3,col='blue',xlab=NA,
         ylab=NA,las=1,cex=0.4)
    lines(xp,yp,col='magenta',lty=4)
    title(xlab=TeX('$\\sigma$'),line=2.5)
    title(ylab=TeX('Marginal posterior for $\\sigma$'),line=2.5)
    text(0.7,0.6,bquote(~nu == .(nu)),col='blue',cex=1.5,pos=4)
    text(0.7,0.1,bquote(~s == .(sr)),col='black',cex=1.5,pos=4)
    text(1.15,1.5,bquote(~mu[N] == .(muSN)),col='magenta',cex=1.5,pos=4)
    text(1.15,1.2,bquote(~sigma[N] == .(sigmaSN)),col='magenta',cex=1.5,pos=4)
    # dev.off()
  }