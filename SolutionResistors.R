print('file: SolutionResistors.R')
# EXERCISE & SOLUTION: replace parallel resistors by 2 resistors with equal resistence
print(' ---------------------------------------------------')
#     Ohm's law J = V/R
#     Ohm's law for parallel set-up: J = J1 + J2 = V/R1 + V/R2
R1 = 600; print(c(R1,'R1 (ohm)'))
R2 = 400; print(c(R2,'R2 (ohm)'))
V  = 220;  print(c(V,'V (volt)'))  # (volt) voltage > 0 (arbitrary value)
J1 = V/R1; print(c(round(J1,4),'J1 (A)'))  # (amperes) electric current
J2 = V/R2; print(c(round(J2,4),'J2 (A)'))               
J = J1+J2; print(c(round(J,4),'J (A)'))                 
Rm = 2*V/J; print(c(Rm,'Rm (ohm)')) 
HarmonicMean = 2/(1/R1+1/R2); print(c(HarmonicMean,'HarmonicMean (ohm)')) 
ArithmeticMean = mean(c(R1,R2)); print(c(ArithmeticMean,'ArithmeticMean (ohm)')) 
print(' ---------------------------------------------------')
xp = c(0,1,1,3,3,5,5,3,3,5,5,7,7,8)
yp = c(0,0,1,1,1.3,1.3,0.7,0.7,1.3,1.3,1,1,0,0)
x1 = c(0,1,1,3);
y1 = c(0,0,1,1)
xr = c(3,3,5,5,3,3,5,5)
yr = c(1,1.3,1.3,0.7,0.7,1.3,1.3,1)
x2 = c(5,7,7,8)
y2 = c(1,1,0,0)
# png('HarmonicMeanResistors220222Sol.png',width=16,height=16,units='cm',res=300)
plot(x1,y1,type='l',lwd=3,col='blue',xlab='',ylab='',las=1,cex=0.4,
     bty='n',xaxt='n',yaxt='n',xlim=c(0,10),ylim=c(-4,1)*1.35)
lines(x1,-y1,lwd=3,col='blue')
lines(x2,y2,lwd=3,col='blue')
lines(x2,-y2,lwd=3,col='blue')
lines(xr,yr,lwd=3,col='black')
lines(xr,-yr,lwd=3,col='black')
text(4,1,'600 ohm',col='black')
text(4,-1,'400 ohm',col='black')
dy=3
lines(x1,y1-dy,lwd=3,col='blue')
lines(x1,-y1-dy,lwd=3,col='blue')
lines(x2,y2-dy,lwd=3,col='blue')
lines(x2,-y2-dy,lwd=3,col='blue')
lines(xr,yr-dy,lwd=3,col='black')
lines(xr,-yr-dy,lwd=3,col='black')
text(4,1-dy,'480 ohm',col='magenta')
text(4,-1-dy,'480 ohm',col='magenta')
# dev.off()
# ' ---------------------------------------------------'
# '600'      'R1 (ohm)'
# '400'      'R2 (ohm)'
# '220'      'V (volt)'
# '0.3667' 'J1 (A)'
# '0.55'   'J2 (A)'
# '0.9167' 'J (A)' 
# '480'      'Rm (ohm)'
# '480'                'HarmonicMean (ohm)'
# '500'                  'ArithmeticMean (ohm)'
# ' ---------------------------------------------------'