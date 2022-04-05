print('file: SolutionMaxEntChiSq5.R')
# EXERCISE & SOLUTION: MaxEnt PDF, mu & E(ln(x)), 0<= x < infinity
# (1) Constraint values:
mu = 5          # mean value
alpha2 = 1.3    # expectation of ln(x)
# (2) Define function determining Lambda_2 (= L2 = z) and plot over
#     a range that includes the root:
myFct1 = function(z){psigamma(z+1, deriv=0)-log((z+1)/mu)-alpha2} # z=L2
L2lower = 0.5; L2upper = 1
L2a = seq(L2lower,L2upper,0.001)
# (3) Numerical calculation of the root Lambda2 = L2:
#     remark: from plot of myFct1 we know that root is crossed in
#             upward direction -> set extendInt to 'upX'
out1 = uniroot(myFct1,lower=L2lower,upper=L2upper,extendInt='upX')
L2 = out1$root
# (4) Calculate the other Lagrange multipliers:
L1 = -(L2+1)/mu
L0 = 1+log( ((-L1)^(L2+1))/ gamma(L2+1) )
# (5) Check constraints:
p = function(x){exp(-1+L0+L1*x+L2*log(x))}
out2=integrate(p,0,Inf)   # normalization
px = function(x){x*exp(-1+L0+L1*x+L2*log(x))}
out3=integrate(px,0,Inf)     # mean
plnx = function(x){log(x)*exp(-1+L0+L1*x+L2*log(x))}
out4=integrate(plnx,0,Inf)   # E(ln(x))
# (6) Plot MaxEnt-PDF:
x = seq(0,20,0.001)
sflag = 2
if (sflag == 2) {
  # png('MaxEntElnxPDF171210.png',width=16,height=16,units='cm',res=300)
  plot(x,p(x),type='l',lwd=4,col='blue',xlab='x',ylab='MaxEnt-PDF',las=1,cex=0.4,cex.lab=1.5)
  xt = 10
  text(xt,0.14,paste('E(x) = ',as.character(mu)),col='blue',pos=4,cex=1.5)
  text(xt,0.12,paste('E(ln(x)) = ',as.character(alpha2)),col='blue',pos=4,cex=1.5)
  # dev.off()
}