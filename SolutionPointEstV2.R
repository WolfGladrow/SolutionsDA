print('file: SolutionPointEstV2.R')
# EXERCISE & SOLUTION: German V2 bombs on London during World War II
fC = c(229,211,93,35,7,1) # frequencies (Clarke, 1946)
NOEC = seq(0,5)      # number of events (bombs/square)
NOBC = 537           # number of bombs (Clarke, 1946)
NOB1 = sum(NOEC*fC)  # number of bombs (calculated from Clarke data)
print('Calculated NOB < NOB given by Clarke ->')
print('1 case with >= 5 bombs actually means 7 bombs')
f = c(229,211,93,35,7,0,0,1) # frequencies (Clarke, 1946, modified)
NOE = seq(0,7)               # number of events
NSq = sum(f)      # number of square
NOB = sum(NOE*f)  # number of bombs
r = NOB/NSq       # mean rate (estimate)
lambdaEst = round(r,2)
# Bayesian estimate:
n = sum(f)
s = sum(NOE*f)
lambdaEstB = (s+1)/n; print(c(round(lambdaEstB,4),'lambdaEstB'))
ulambdaEstB = sqrt((s+1))/n; print(c(round(ulambdaEstB,4),'ulambdaEstB'))
lambdaEstBr = round(lambdaEstB,3); ulambdaEstBr = round(ulambdaEstB,3)
print('Predict frequencies based on Poisson distribution:')
fP = NSq*dpois(NOE,lambdaEstB)
# png('PoissonV2London180107.png',width=16,height=16,units='cm',res=300)
plot(NOE,log10(f),type='p',lwd=4,col='black',xlab='Number of bombs per square',
     ylab=NA,las=1,cex=0.6,cex.lab=1.5,ylim=c(-2,2.5))
title(ylab=TeX('$log_{10}(frequency)$'),line=2.3,cex.lab=1.5)
points(NOE,log10(fP),lwd=4,col='magenta',cex=0.6,pch=24)
text(0.5,-1,pos=4,bquote(~bar(x) == .(lambdaEst)),col='magenta',cex=1.5)
text(0.5,0,pos=4,bquote(~hat(lambda) == .(lambdaEstBr)  %+-% .(ulambdaEstBr)),col='magenta',cex=1.5)
# dev.off()
