print('file: SolutionMDintegralMultiple.R')
# EXERCISE & SOLUTION: double integral over f(x,y; alpha, beta): covariance matrix
# f(x,y; \alpha, \beta) = \frac{24}{4\alpha + 3\beta}(\alpha x^2 y + \beta x y^3)
alpha=3;beta=2
# x-variance (sigma_x^2):
f4=function(z,alpha,beta){x=z[1]; y=z[2];
return((x-(3*alpha+2*beta)/(4*alpha+3*beta))^2*
         24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
library(cubature)
out4=adaptIntegrate(f4,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)
varxnum = out4$integral # 0.04506173
# y-variance (sigma_y^2):
f5=function(z,alpha,beta){x=z[1]; y=z[2];
return((y-4/15*(10*alpha+9*beta)/(4*alpha+3*beta))^2*
         24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
out5=adaptIntegrate(f5,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)
varynum = out5$integral # 0.04987654
# covariance:
f6=function(z,alpha,beta){x=z[1]; y=z[2];
return((x-(3*alpha+2*beta)/(4*alpha+3*beta))*
         (y-4/15*(10*alpha+9*beta)/(4*alpha+3*beta))*
         24/(4*alpha+3*beta)*(alpha*x^2*y+beta*x*y^3))}
out6=adaptIntegrate(f6,lowerLimit=c(0,0),upperLimit=c(1,1),alpha,beta)
covarnum = out6$integral # -0.002469136
# --------------------------------------------------------------------
# plot integrands:
xa = seq(0,1,0.02); ya = xa; L = length(xa)
f = matrix(data=NA,nrow=L,ncol=L)
# install.packages('plot3D')
library(plot3D)
sflag = 3
if (sflag == 1) {  # cov(x,x)
  for(m in 1:L) {
    for(n in 1:L) f[m,n] = f4(c(xa[m],ya[n]),alpha,beta)}
  max(f)
  # png('jointIcovxx3D170220.png',width=16,height=16,units='cm',res=300)
  persp3D(z=f,main='',clab='',zlab='Integrand',breaks = seq(0,0.6,0.05),
          ticktype='detailed',nticks=2,cex.lab=1.5)  # ,clab = 'PDF'
  # dev.off()
}
if (sflag == 2) {  # cov(y,y)
  for(m in 1:L) {
    for(n in 1:L) f[m,n] = f5(c(xa[m],ya[n]),alpha,beta)}
  max(f)
  # png('jointIcovyy3D170220.png',width=16,height=16,units='cm',res=300)
  persp3D(z=f,main='',clab='',zlab='Integrand',breaks = seq(0,0.6,0.05),
          ticktype='detailed',nticks=2,cex.lab=1.5)
  # dev.off()
}
if (sflag == 3) {  # cov(x,y)
  for(m in 1:L) {
    for(n in 1:L) f[m,n] = f6(c(xa[m],ya[n]),alpha,beta)}
  # png('jointIcovxy3D170220.png',width=16,height=16,units='cm',res=300)
  persp3D(z=f,main='',clab='',zlab='Integrand',breaks = seq(-0.2,0.6,0.05),
          ticktype='detailed',nticks=2,cex.lab=1.5)
  # dev.off()
}