print('file: SolutionO2CO2Keeling.R')
# EXERCISE & SOLUTION: Keeling (1988, Table II) correlation tests
CO2 = c(12.5,17.0,13.5,18.3,60.0,55.8,62.2,56.2,49.3,42.1,
        27.6,21.4,26.2,28.8,33.1,24.3,17.0,4.4,7.0,3.9,9.3,
        14.1,2.8,4.9,4.9)
O2 = c(-5.6,-8.4,-6.3,-9.6,-62.9,-51.8,-60.6,-53.2,-48.6,-37.0,
       -18.6,-12.2,-14.8,-19.6,-29.9,-16.9,-7.9,1.2,-1.3,6.8,
       -1.0,-7.2,6.5,5.0,7.3)
n = length(CO2); print(c(n,'n'))
r = cor(CO2,O2); print(c(round(r,4),'r'))
out1 = cor.test(CO2,O2); p1 = out$p.value; print(c(p1,'p1'))
library(fBasics)
out2 = correlationTest(CO2,O2); p2 = 2.2e-16; print(c(p2,'p2'))
out3 = pearsonTest(CO2,O2); p3 = 2.2e-16; print(c(p3,'p3'))
# png('Keeling88Fig5cor.png',width=16,height=12,units='cm',res=300)
plot(CO2,O2,type='p',lwd=4,col='blue',
     xlab='dX_CO2 (ppm)',ylab='dX_O2 (ppm)',las=1,cex=0.6,cex.lab=1.5)
# dev.off()