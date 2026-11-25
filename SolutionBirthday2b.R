print('file: SolutionBirthday2b.R')
print(date())
print(R.Version()$version.string)
# ---------------------------------------------------------
# created by: Dieter.Wolf-Gladrow@awi.de 
#    11/2026 
# ---------------------------------------------------------
Narr = seq(1,50); L = length(Narr)
pb = numeric(L)
j=1; pb[j] = 0
for(j in 2:L) {N = Narr[j]; pnbN = 1;
  for(k in 1:N) pnbN = pnbN*(366-k)/365; # probability for no joint birthday
  pb[j] = 1-pnbN}  # probability for joint birthday
  # --- 50% chance:
  k50 = which.min((pb-0.5)^2)
  pb[k50]
  k90 = which.min((pb-0.9)^2)
  pb[k90]
  print(c(round(pb[k50],3),'pb[k50]'))
  print(c(Narr[k50],'Narr[k50]'))
  print(c(round(pb[k90],3),'pb[k90]'))
  print(c(Narr[k90],'Narr[k90]'))
  k1 = 26
  print(c(round(pb[k1],3),'pb[k1]'))
  print(c(Narr[k1],'Narr[k1]'))
  # png('Birthday261125.png',width=16,height=12,units='cm',res=300)
  plot(Narr,pb,type='p',lwd=3,col='blue',
       xlab='Number of persons N',
       ylab='Probability for joint birthday',las=1,cex=0.4)
  abline(0.5,0,col='red')
  xp = c(k50,k50); yp = c(-0.1,0.5)
  lines(xp,yp,col='red')
  text(10,0.55,paste('N = ',as.character(k50)),
                     col='red',pos=4)
  abline(0.9,0,col='black')
  xp = c(k90,k90); yp = c(-0.1,0.9)
  lines(xp,yp,col='black')
  text(27,0.94,paste('N = ',as.character(k90)),
       col='black',pos=4)
# dev.off()
# ---------------------------------------------------
# Results: 
  # [1] "file: SolutionBirthday2b.R"
  # [1] "Wed Nov 25 20:07:57 2026"
  # [1] "R version 4.4.2 (2024-10-31)"
  # [1] "0.507"   "pb[k50]"
  # [1] "23"        "Narr[k50]"
  # [1] "0.903"   "pb[k90]"
  # [1] "41"        "Narr[k90]"
  # [1] "0.598"  "pb[k1]"
  # [1] "26"       "Narr[k1]"
  # ---------------------------------------------------