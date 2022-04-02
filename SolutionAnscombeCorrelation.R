print('file: SolutionAnscombeCorrelation.R')
# EXERCISE & SOLUTION: Anscombe's quartet (1973) mean, correlation:
data1 = c(10.0,8.04,10.0,9.14,10.0,7.46,8.0,6.58,
          8.0,  6.95,  8.0, 	8.14,  8.0,  6.77, 	8.0, 	5.76,
          13.0,  7.58, 13.0, 	8.74, 13.0, 12.74, 	8.0, 	7.71,
          9.0,  8.81, 	9.0, 	8.77,  9.0,  7.11, 	8.0, 	8.84,
          11.0,  8.33, 11.0, 	9.26, 11.0,  7.81, 	8.0, 	8.47,
          14.0,  9.96, 14.0, 	8.10, 14.0,  8.84, 	8.0, 	7.04,
          6.0,  7.24, 	6.0, 	6.13,  6.0,  6.08, 	8.0, 	5.25,
          4.0,  4.26, 	4.0, 	3.10,  4.0,  5.39, 19.0, 12.50,
          12.0, 10.84, 12.0, 	9.13, 12.0,  8.15, 	8.0, 	5.56,
          7.0,  4.82, 	7.0, 	7.26,  7.0,  6.42, 	8.0, 	7.91,
          5.0,  5.68, 	5.0, 	4.74,  5.0,  5.73, 	8.0, 	6.89)
M1 = matrix(data=data1,nrow=8,ncol=11)
x1=M1[1,]; y1=M1[2,]; x2=M1[3,]; y2=M1[4,]
x3=M1[5,]; y3=M1[6,]; x4=M1[7,]; y4=M1[8,]
# ---------------------------------------------------------------------
  meanx1 = mean(x1); print(c(meanx1,'meanx1'))
  meanx2 = mean(x2); print(c(meanx2,'meanx2'))
  meanx3 = mean(x3); print(c(meanx3,'meanx3'))
  meanx4 = mean(x4); print(c(meanx4,'meanx4'))
  varx1 = var(x1); print(c(varx1,'varx1'))
  varx2 = var(x2); print(c(varx2,'varx2'))
  varx3 = var(x3); print(c(varx3,'varx3'))
  varx4 = var(x4); print(c(varx4,'varx4'))
  meany1 = mean(y1); print(c(round(meany1,4),'meany1'))
  meany2 = mean(y2); print(c(round(meany2,4),'meany2'))
  meany3 = mean(y3); print(c(round(meany3,4),'meany3'))
  meany4 = mean(y4); print(c(round(meany4,4),'meany4'))
  vary1 = var(y1); print(c(round(vary1,4),'vary1'))
  vary2 = var(y2); print(c(round(vary2,4),'vary2'))
  vary3 = var(y3); print(c(round(vary3,4),'vary3'))
  vary4 = var(y4); print(c(round(vary4,4),'vary4'))
  cor1 = cor(x1,y1); print(c(round(cor1,4),'cor1')) 
  cor2 = cor(x2,y2); print(c(round(cor2,4),'cor2')) 
  cor3 = cor(x3,y3); print(c(round(cor3,4),'cor3'))
  cor4 = cor(x4,y4); print(c(round(cor4,4),'cor4')) 
  cov1 = cov(x1,y1); print(c(round(cov1,4),'cov1'))
  cov2 = cov(x2,y2); print(c(round(cov2,4),'cov2'))
  cov3 = cov(x3,y3); print(c(round(cov3,4),'cov3'))
  cov4 = cov(x4,y4); print(c(round(cov4,4),'cov4'))
  # pedestrian way:
  cor1p = sum((x1-meanx1)*(y1-meany1))/sqrt(sum((x1-meanx1)^2))/sqrt(sum((y1-meany1)^2))
  print(c(round(cor1p,4),'cor1p')) 
# --------------------------------------------------------------------------------------
# 'file: SolutionAnscombeCorrelation.R'
# '9'      'meanx1'
# '9'      'meanx2'
# '9'      'meanx3'
# '9'      'meanx4'
# '11'    'varx1'
# '11'    'varx2'
# '11'    'varx3'
# '11'    'varx4'
# '7.5009' 'meany1'
# '7.5009' 'meany2'
# '7.5'    'meany3'
# '7.5009' 'meany4'
# '4.1273' 'vary1' 
# '4.1276' 'vary2' 
# '4.1226' 'vary3' 
# '4.1232' 'vary4' 
# '0.8164' 'cor1'  
# '0.8162' 'cor2'  
# '0.8163' 'cor3'  
# '0.8165' 'cor4'  
# '5.501' 'cov1' 
# '5.5'  'cov2'
# '5.497' 'cov3' 
# '5.499' 'cov4' 
# '0.8164' 'cor1p' 

# ----------------------------------