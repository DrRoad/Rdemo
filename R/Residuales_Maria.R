##Residuales mod1, mod2 y mod 3

#Residuales mod1

r1 = mod1$residuals

par(mfrow=c(2,2))
plot(t,
     r1,
     type='o',
     ylab='residuo')
abline(h=0,lty=2)
plot(density(r1),
     xlab='x',
     main= '')
qqnorm(r1)
qqline(r1,col=2)
