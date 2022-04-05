print('file: SolutionProb3KineticEnergy.R')
# EXERCISE & SOLUTION: particles with 3 kinetic energies
# The mean energy Emean can vary between 1 and 9. Calculate and
# plot the probabilities pj, j=1,2,3 over this energy range.
Ea = seq(1,9,0.1); L = length(Ea)
j = seq(1,3)
p1 = numeric(L); p2 = numeric(L); p3 = numeric(L)
for(k in 1:L) {Emean = Ea[k];
myFct1 = function(z){1+z^3+z^8-(1+4*z^3+9*z^8)/Emean};
zlower=0.8; zupper=1;
out1 = uniroot(myFct1,lower=zlower,upper=zupper,extendInt='downX')
z1 = out1$root;
lambda1 = log(z1)
lambda0 = 1+log(1/sum(exp(lambda1*j^2)));
p1[k] = exp(-1+lambda0+lambda1);
p2[k] = exp(-1+lambda0+lambda1*4);
p3[k] = exp(-1+lambda0+lambda1*9)}
library(latex2exp)
# png('E3particles171213.png',width=16,height=16,units='cm',res=300)
plot(Ea,p1,type='l',lwd=3,col='blue',xlab='Mean energy',
     ylab=NA,las=1,cex=0.4,cex.lab=1.5)
title(ylab=TeX('$Probabilities\\, p_1,\\, p_2,\\, p_3$'),line=2.5,cex.lab=1.5)
lines(Ea,p2,col='black',lwd=3,lty=2)
lines(Ea,p3,col='red',lwd=3,lty=4)
# dev.off()
# ---------------------------------------------------
# Results: 
EatequalProb = 1/3*(1+4+9)
print(c('EatequalProb = ',EatequalProb))