print('file: SolutionPDs-F-versus-Normal.R')
# EXERCISE & Solution: compare F & normal
dx=0.01; xarr = seq(dx,2,dx)
nu1=100; nu2=100   # degrees of freedom
fPDF = df(x=xarr,df1=nu1,df2=nu2);
mu = nu2/(nu2-2); print(c(round(mu,4),'mu'))
sigma = sqrt(2*nu2^2*(nu1+nu2-2)/nu1/(nu2-2)^2/(nu2-4)); print(c(round(sigma,4),'sigma'))
muEst = sum(xarr*fPDF)*dx; print(c(round(muEst,4),'muEst'))
sigEst = sqrt(sum((xarr-muEst)^2*fPDF)*dx); print(c(round(sigEst,4),'sigEst'))
N = dnorm(x=xarr,mean=mu,sd=sigma);
# png('ExerFandNormal220228.png',width=16,height=16,units='cm',res=300)
plot(xarr,fPDF,type='l',lwd=3,col='blue',xlab='x',ylab='Density',las=1,cex.lab=1.5)
lines(xarr,N,col='red',lwd=3,lty=2)
legend('topleft',legend=c('F','normal'),col=c('blue','red'),
       lty=c(1,2),lwd=c(3,3),cex=1.5)
# dev.off()