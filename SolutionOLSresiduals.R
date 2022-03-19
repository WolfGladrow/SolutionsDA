print('file: SolutionOLSresiduals.R')
# EXERCISE & SOLUTION: residuals of OLS, NO3 over PO4, whole ocean
infoNO3.nc = nc_open('nitrate_annual_1deg.nc')
infoPO4.nc = nc_open('phosphate_annual_1deg.nc')
NO3 = ncvar_get( infoNO3.nc, 'n_an')
PO4 = ncvar_get( infoPO4.nc, 'p_an')
# Remove 'na' (not available) values:
N = 360*180*33 # number of data (including not avaible ('na') data)
NO3clean = numeric(N); PO4clean = numeric(N)
c=0;
for (k in 1:N) {if ( (is.na(NO3[k]) == FALSE) && (is.na(PO4[k]) == FALSE) )
{ if (PO4[k] < 4){ c=c+1; NO3clean[c] = NO3[k]; PO4clean[c] = PO4[k]}}}
NO3c = numeric(c); PO4c = numeric(c)
NO3c[1:c] = NO3clean[1:c]; PO4c[1:c] = PO4clean[1:c]
print(c('c = ',c))
# Fit straight line to the data: odinary least squares (OLS)
out = lm(NO3c ~ PO4c)
out1 = summary(out)
# residuals EXERCISE:
r = out1$residuals
sflag = 1
if(sflag == 1) {
  # png('NO3vsPO4Resi171226.png',width=16,height=12,units='cm',res=300)
  plot(density(r,from=-5,to=5),col='blue',xlab='Residuals',
       ylab='Density',lwd=3,main='',las=1,cex.lab=1.5,xlim=c(-4,4))
  xp = seq(-5,5,0.01)
  yp = dnorm(xp,mean=mean(r),sd=sd(r))
  lines(xp,yp,col='yellow',lwd=2,lty=1)
  lines(xp,yp,col='black',lwd=3,lty=2)
  legend('topleft',legend=c('estimate','normal apprimation'),col=c('blue','black'),
         lty=c(1,2),lwd=c(3,3),cex=1.2)
  # dev.off()
}
