print('file: SolutionRollingDieHist.R')
# EXERCISE & Solution: random sample from discrete uniform distribution (rolling a die)')
set.seed(1953) # set seed for random number generators
xrange = seq(1,6)
hrange = seq(0,6)
k = sample(x=xrange,10000,replace=TRUE);
# png('RandomDieExercise160721.png',width=16,height=12,units='cm',res=300)
hist(k,breaks=hrange,col='blue',main='',las=1,cex.lab=1.5)
# dev.off()