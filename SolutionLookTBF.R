print('file: SolutionLookTBF.R')
# EXERCISE & SOLUTION: Transparent boxplot for random sample from F-PDF
set.seed(1953) # set seed for random number generators
L = 30; x = rf(L,15,3)
# png('BoxPlotTranparentF220222.png',width=16,height=16,units='cm',res=300)
boxplot(x,col='yellow',las=1,xlab='x',cex.lab=1.5) 
set.seed(1953) # set seed for random number generators
jitter = rnorm(L,1,0.05)
points(jitter,x,col='blue',lwd=3,cex=0.4)
# dev.off()