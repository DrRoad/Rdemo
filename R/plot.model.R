library(dataseries)
library(forecast)
library(ggplot2)

plot.model <- function(
  datos,                  #Columna de datos de la serie de tiempo
  fechas,                 #Columna de fechas de la serie de tiempo
  frequency,              #Frecuencua atribuida a la serie de tiempo
  m,                      #Cantidad de periodos a pronosticar
  año.inicio,             #Año inicio estimacion
  periodo.inicio,         #Periodo inicio estimacion
  number.model            #Numero modelo a seleccionar
){

  y <- ts(data = datos,frequency = frequency, start = c(año.inicio, periodo.inicio))

  n <- length(y)

  yi <- ts(y[1:(n-m)],frequency=frequency)

  fechas.model <- fechas[1:(n-m)]
  ti <- seq(1:length(yi))
  ti2 <- ti*ti
  ti3 <- ti*ti*ti
  It <- seasonaldummy(yi)

  if (number.model == 1){
    model <- lm(yi ~ ti)
  } else if (number.model == 2){
    model <- lm(yi ~ ti + ti2)
  } else if (number.model == 3){
    model <- lm(yi ~ ti + ti2 + ti3)
  } else if (number.model == 4) {
    model <- lm(yi ~ ti + It)
  } else if (number.model == 5){
    model <- lm(yi ~ ti + ti2 + It)
  } else {
    model <- lm(yi ~ ti + ti2 + ti3 + It)
  }

  y.fit <- model$fitted.values

  ggplot(data.frame(fechas.model,yi,y.fit))+
    geom_line(aes(fechas.model,yi),col='blue')+
    geom_line(aes(fechas.model,y.fit),col='red')+
    ylab('Data')+
    xlab('Time')+
    scale_y_continuous(expand = c(0,0))
}
