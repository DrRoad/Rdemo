output$resplot <- renderPlot({

  req(input$file)
  df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
  datos <- df[,ncol(df)]

if(model == 3){
  par(mfrow=c(2,2)) #Divido el area del grafico en 2 filas y 2 columnas
  options(repr.plot.width=10, repr.plot.height=6) #Ajusto el ancho del grafico
  r3=m3$residuals
  plot(t,r3, type='l', ylab='',main="Residuales Modelo Cubico",col="red") #residuales para modelo cubico
  abline(h=0,lty=2)
  plot(density(r3),xlab='x', main="Densidad Residuales Modelo Cubico",col="red") #densidad para modelo cubico
  qqnorm(r3)               # Grafica qqnorm para probar normalidad
  qqline(r3,col=2)
  acf(r3, ci.type="ma",60) # Prueba ACF

} else if(model == 2){
  par(mfrow=c(2,2))
  options(repr.plot.width=10, repr.plot.height=6)
  r2=m2$residuals
  plot(t,r2, type='l', ylab='',main="Residuales Modelo Cuadratico",col="red")
  abline(h=0,lty=2)
  plot(density(r2),xlab='x', main="Densidad Residuales Modelo Cuadratico",col="red")
  qqnorm(r2)               # Grafica qqnorm para probar normalidad
  qqline(r2,col=2)
  acf(r2, ci.type="ma",60) # Prueba ACF

} else {
  par(mfrow=c(2,2))
  options(repr.plot.width=10, repr.plot.height=6)
  r1=m1$residuals
  plot(t,r1, type='l', ylab='',main="Residuales Modelo Lineal",col="red")
  abline(h=0,lty=2)
  plot(density(r1),xlab='x', main="Densidad Residuales Modelo Lineal",col="red")
  qqnorm(r1)               # Grafica qqnorm para probar normalidad
  qqline(r1,col=2)
  acf(r1, ci.type="ma",60) # Prueba ACF
}
