print('file: SolutionGafarianCB.R')
# EXERCISE & SOLUTION: narrow interval confidence bands
# Casella02Berger: Table 11.3.1 p.542 (xns = x not sorted)
xns = c(3.74,3.66,0.78,2.40,2.18,1.93,0.20,2.50,3.50,1.35,2.36,3.13,
        1.22,1.00,1.29,0.95,1.05,2.92,1.76,0.51,2.17,1.99,1.53,2.60)
yns = c(3.22,4.87,0.12,2.31,4.25,2.24,2.81,3.71,3.11,0.90,4.39,4.36,
        1.23,3.13,4.05,2.28,3.60,5.39,4.12,3.16,4.40,1.18,2.54,4.89)
n = length(xns); print(c(n,'sample size n'))
xmean = mean(xns); print(c(round(xmean,2),'xmean'))
ymean = mean(yns); print(c(round(ymean,2),'ymean'))
Sxx = sum((xns-xmean)^2); print(c(round(Sxx,2),'Sxx'))
# -------------------------------------------------------------------
# Sort data & simple linear regression (SLM):
sx = order(xns); x = xns[sx]; y = yns[sx] 
outSLR = lm(y ~ x)
b0 = as.numeric(outSLR$coefficients[1]); print(c(round(b0,4),'intercept (estimate)'))
b1 = as.numeric(outSLR$coefficients[2]); print(c(round(b1,4),'slope (estimate)'))
r = outSLR$residuals
# ------------------------------------------------------------------
# Normally distributed residuals? Perform Shapiro-Wilk test: 
outST = shapiro.test(r)
pST = outST$p.value; print(c(round(pST,4),'p-value Shapiro-Wilk test'))
if (pST < 0.05) print('Reject H0 (residuals from normal PDF), alpha = 0.05')
if (pST >= 0.05) print('Do not reject H0 (residuals from normal PDF), alpha = 0.05')
S = sqrt(sum(r^2)/(n-2)); print(c(round(S,4),'S=standard error of the regression'))
# Gafarian (1964, p.187)
# ---------------------------------------------------
alpha = 0.1; print(c(alpha,'alpha (as in Casella & Berger, 2002)'))
# ---------------------------------------------------
# Confidence bands: single point confidence interval approximation:
# hyperbolic
nures = n-2   # degrees of freedom 
tc = qt(1-alpha/2,nures); print(c(round(tc,4),'tc'))
SE = S*sqrt((1/n+(x-xmean)^2/Sxx))
CB1U = b0+b1*x+tc*SE # upper 90\% confidence band
CB1L = b0+b1*x-tc*SE # lower 90\% confidence band
# ---------------------------------------------------
# Confidence bands: (2) Scheffe (1959): hyperbolic
# Casella \& Berger (2002, p.560, Eq. 11.3.43)
Malpha = sqrt(2*qf(1-alpha,2,n-2)); print(c(round(Malpha,4),'Malpha'))
print('or Malpha = sqrt(2*qf(alpha,2,n-2,lower.tail=FALSE))')
CB2U = b0+b1*x+Malpha*S*sqrt(1/n+(x-xmean)^2/Sxx)
CB2L = b0+b1*x-Malpha*S*sqrt(1/n+(x-xmean)^2/Sxx)
# ---------------------------------------------------
# Confidence interval at x = x0: single point confidence interval
x0 = 1.2
SE0 = S*sqrt((1/n+(x0-xmean)^2/Sxx))
xpCI = c(x0,x0)
CI1x1 = c(b0+b1*x0-tc*SE0,b0+b1*x0+tc*SE0)
# ---------------------------------------------------
# Gafarian (1964) confidence bands: straight
# ---------------------------------------------------
xU = x0+0.1; xL = x0-0.1; print(c(xL,xU,'xL,xU'))
xNarrow = seq(xL,xU,0.01)
sigXi = sqrt(sum((x-xmean)^2)/n); print(c(round(sigXi,4),'sigXi = sd of x = v in M81'))
onemalpha = 1 - alpha
dalpha = alpha/500; print(c(dalpha,'dalpha'))
# ---------------------------------------------------
# Bowden & Graybill (1966):
# ---------------------------------------------------
IntegrandBG66 = function(z,n,rho) {u=z[1]; v=z[2];
return(1/(2*pi*sqrt(1-rho^2))*(1+(u^2-2*rho*u*v+v^2)/((n-2)*(1-rho^2)))^(-n/2))}
# install.packages('cubature')
library(cubature)
# ---------------------------------------------------
Dg = 3 # guess D
gB66 = sqrt(1/n+(xL-xmean)^2/(n*sigXi^2))
hB66 = sqrt(1/n+(xU-xmean)^2/(n*sigXi^2))
AB66 = gB66/hB66; print(c(round(AB66,4),'AB66'))
rho = (1+(xL-xmean)*(xU-xmean)/sigXi^2)/(sqrt(1+(xL-xmean)^2/sigXi^2)*
                                           sqrt(1+(xU-xmean)^2/sigXi^2))
print(c(round(rho,4),'rho'))
A = AB66; print(c(round(A,4),'A'))
dalpha = alpha/500; print(c(dalpha,'dalpha'))
deltaDg = 0.1; print(c(deltaDg,'deltaDg'))  # 1. decimal
k = 0; out = 0
while ((k < 20) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),
                      upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
     print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
deltaDg = 0.01; print(c(deltaDg,'deltaDg'))  # 2. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),
                      upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
     print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
deltaDg = 0.001; print(c(deltaDg,'deltaDg'))  # 3. decimal
k = 0; out = 0
while ((k < 10) && (abs(out-onemalpha) > dalpha)) {
  k = k+1;
  if(out > onemalpha) Dg = Dg-deltaDg
  if(out < onemalpha) Dg = Dg+deltaDg
  out4=adaptIntegrate(IntegrandBG66,lowerLimit=c(-Dg,-A*Dg),upperLimit=c(Dg,A*Dg),n,rho)
  out = out4$integral;
  if(abs(out-onemalpha) < dalpha) {k = 1000; 
     print(c(k,out-onemalpha,'k,out-onemalpha'))}
}
print(c(k,Dg,'k,Dg'))
print(c(round(Dg,4),'D (final) = half-width (Bowden66Graybill)'))
print(c(round(Dg,2),'D (final) = half-width (Bowden66Graybill)'))
# print('B66,Table3,alpha=0.05,n-2=20,A=1,|rh0|=0.3: D=2.40')
print('B66,Table3,alpha=0.1,n-2=20,A=1,|rh0|=0.3: D=2.05')
deltaB66 = Dg*gB66; 
print(c(round(deltaB66,4),'deltaB66 = Cstar (Bowden66Graybill)'))
print(c(round(deltaB66*S,4),'deltaB66*S = Bowden66 half-width'))
# ---------------------------------------------------
print(c(round(deltaB66*S,4),'deltaB66*S = Gafarian half-width (Bowden66)'))
CBBow66U = b0+b1*xNarrow+deltaB66*S
CBBow66L = b0+b1*xNarrow-deltaB66*S
# ---------------------------------------------------
sflag = 2
if (sflag == 2) { # data \& Scheffe \& Gafarian \& single point CI
  # png('ConfidenceBands200628NI.png',width=16,height=12,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  abline(outSLR,col='blue')
  lines(xpCI,CI1x1,col='red',lty=1) # single point CI
  lines(x,CB2U,col='red',lty=1)     # Scheffe
  lines(x,CB2L,col='red',lty=1)
  lines(xNarrow,CBBow66U,col='black',lty=1) # Gafarian (Bowden \& Greybill, 1966)
  lines(xNarrow,CBBow66L,col='black',lty=1)
  # dev.off()
}
# lines(x,CBGafU,col='black',lty=1) # Gafarian
# lines(x,CBGafL,col='black',lty=1)