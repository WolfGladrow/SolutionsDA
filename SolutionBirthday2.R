print('file: SolutionBirthday2.R')
# EXERCISE & SOLUTION: at least 2 persons birthday on same day(s)
n = 26
pnbN = 1;
for(k in 2:n) pnbN = pnbN*(366-k)/365 # probability for no joint birthday
pb = 1-pnbN       # probability for joint birthday
print(c(n,'number of persons'))
print(c(pb,'pb'))
print(c(round(pb*100,1),'probability (%)')) # 59.8% !!!