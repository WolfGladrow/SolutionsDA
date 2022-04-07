print('file: SolutionNHST-Mendel-Fisher.R')
# EXERCISE & SOLUTION: Example from Fisher (1925, Table 12) Mendelian class frequencies
fobs = c(328,122,77,33)       # observed frequencies m+x
total = sum(fobs)
H0 = c(3,1)
fexp = numeric(4)
total1 = sum(fobs[1:2]); total2 = sum(fobs[3:4])
fexp[1:2] = round(total1*H0/sum(H0),1) # expected frequencies m
fexp[3:4] = round(total2*H0/sum(H0),1) # expected frequencies m
x = fobs-fexp
q = round(x^2/fexp,3)
totalq = sum(q)         # test statistic chi-squared
nu = length(fexp) - 2   # degrees of freedom
alpha = 0.05; print(c(alpha,' alpha: chosen level of significance'))
chisqc = qchisq(p=(1-alpha),df=nu)
print(c(round(chisqc,2),' critical chi-square value for alpha'))
p = (1-pchisq(q=totalq,df=nu)); print(c(round(p,4),' p-value'))
if(p < alpha) print(c('p < alpha = ',alpha,': reject H0'))
if(p >= alpha) print(c('p >= alpha = ',alpha,': do not reject H0'))
xp = seq(1,4)
# png('MendelianExercise190415b.png',width=16,height=16,units='cm',res=300)
plot(xp,fobs,type='p',lwd=4,col='blue',xlab='Sample',ylab='# of individuals',
     las=1,cex=0.6,ylim=c(0,350),cex.lab=1.5)
points(xp,fexp,col='magenta',lwd=4,pch=24,cex=0.6)
# dev.off()