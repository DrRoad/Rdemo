##Modelos: 1-tendencia: lineal, cuadrático, cúbico
##2-tendencia + estacionalidad: lineal, cuadrático, cúbico, todos con indicadoras

##Variables de entrada
#y: vector de datos de la serie de tiempo
#frecuencia: de toda la serie de tiempo
#añoinicio: de toda la serie de tiempo
#añofin: de toda la serie de tiempo
#unidaddetiempoinicio: de inicio de de toda la serie de tiempo, teniendo en cuenta la frecuencia ya definida
#unidaddetiempofin: de fin de toda la serie de tiempo, teniendo en cuenta la frecuencia ya definida
#periodosapronosticar: según la frecuencia ya definida

library(forecast)
source("medidas2.r") #Lo copié igual de Norman, evaluar cuáles cálculos dejar

#Datos como una serie de tiempo

y <- ts(y,
        frequency=frecuencia,
        start=c(añoinicio,unidaddetiempoinicio),
        end=c(añofin,unidaddetiempofin))

#Gráfica de la serie de tiempo

options(repr.plot.width=10, repr.plot.height=4) #Ajustar de ser necesario
plot.ts(y,
        main="Serie de tiempo",
        type='l',
        lwd=2,
        col='navy',
        xlab='Fechas',
        ylab='Valores observados' )

#Gráfica stl opcional

options(repr.plot.width=8, repr.plot.height=4) #Ajustar de ser necesario
p = stl(y, s.window = 'per', t.window = 50)
plot(p, lwd = 2, col = 'gray45')

#Datos para la validación cruzada

m = periodosapronosticar
T = length(y)
yi = y[1:(T-m)]
yf = y[(T-m+1):T]

##Mod1: tendencia lineal

#Ajuste mod1

t = seq(1:(T-m))
mod1 = lm(yi~t)
summary(mod1)

##Mod2: tendencia cuadrática

#Ajuste mod2

t2 = t^2
mod2 = lm(yi~t2)
summary(mod2)

##Mod3: tendencia cúbica

#Ajuste mod3

t3 = t^3
mod3 = lm(yi~t3)
summary(mod3)

##Comparaciones de ajuste mod1, mod2 y mod3

Modelo.lineal = medidas(mod1,yi,2)
Modelo.cuadrático = medidas(mod2,yi,3)
Modelo.cúbico = medidas(mod3,yi,4)

M = cbind(Modelo.lineal,Modelo.cuadrático,Modelo.cúbico)

M

##Gráficas datos observados versus datos ajustados

#Guardar valores estimados

yhat1 = mod1$fitted.values
yhat2 = mod2$fitted.values
yhat3 = mod3$fitted.values

#Gráfica mod1

options(repr.plot.width=8, repr.plot.height=6) #Ajustar de ser necesario
plot(t, yi, type = "l", lwd = 2, ylim = c(0,800)) #Ajustar ylim y xlim si es necesario
lines(t, yhat1, col = "red", lwd = 2)
legend( "topright",
        c("Datos observados","Datos ajustados mod1"),
        lwd = c(2, 2),
        col = c('black','red'),
        bty = "n")
grid()

#Gráfica mod2

options(repr.plot.width=8, repr.plot.height=6) #Ajustar de ser necesario
plot(t, yi, type = "l", lwd = 2, ylim = c(0,800)) #Ajustar ylim y xlim si es necesario
lines(t, yhat2, col = "red2", lwd = 2)
legend( "topright",
        c("Datos observados","Datos ajustados mod2"),
        lwd = c(2, 2),
        col = c('black','red2'),
        bty = "n")
grid()

#Gráfica mod3

options(repr.plot.width=8, repr.plot.height=6) #Ajustar de ser necesario
plot(t, yi, type = "l", lwd = 2, ylim = c(0,800)) #Ajustar ylim y xlim si es necesario
lines(t, yhat3, col = "red3", lwd = 2)
legend( "topright",
        c("Datos observados","Datos ajustados mod3"),
        lwd = c(2, 2),
        col = c('black','red3'),
        bty = "n")
grid()

##Faltan pronósticos de estos tres mod, gráficas y medidas
##Faltan los residuales de estos tres mod: en otro script o aquí mismo ?
##adelanto en otro script Residuales_Maria

##Falta todo para los demás tres mod
