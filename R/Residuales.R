radioButtons('modelo', 'Seleccione el modelo de ajuste:',
             c(Lineal='1', Cuadratico='2', Cubico='3'))


## Salidas Analisis de Residuales
output$resplot <- renderPlot({

  req(input$file)
  df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
  datos <- df[,ncol(df)]


  #Modelos de Ajuste
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


  #Graficación Modelos de Ajuste

  par(mfrow=c(2,2))
  options(repr.plot.width=10, repr.plot.height=6) #Ajusto el ancho del grafico

if(model == 3){
  r3=mod3$residuals
  plot(t,r3, type='l', ylab='',main="Residuales Modelo Cubico",col="green") #residuales para modelo cubico
  abline(h=0,lty=2)
  plot(density(r3),xlab='x', main="Densidad Residuales Modelo Cubico",col="green") #densidad para modelo cubico
  qqnorm(r3)               # Grafica para realizar prueba de normalidad
  qqline(r3,col=2)
  acf(r3, ci.type="ma",60) # Prueba ACF (autocorrelación)

} else if(model == 2){
  r2=mod2$residuals
  plot(t,r2, type='l', ylab='',main="Residuales Modelo Cuadratico",col="green")
  abline(h=0,lty=2)
  plot(density(r2),xlab='x', main="Densidad Residuales Modelo Cuadratico",col="green")
  qqnorm(r2)               # Grafica para realizar prueba de normalidad
  qqline(r2,col=2)
  acf(r2, ci.type="ma",60) # Prueba ACF (autocorrelación)

} else {
  r1=mod1$residuals
  plot(t,r1, type='l', ylab='',main="Residuales Modelo Lineal",col="green")
  abline(h=0,lty=2)
  plot(density(r1),xlab='x', main="Densidad Residuales Modelo Lineal",col="green")
  qqnorm(r1)               # Grafica para realizar prueba de normalidad
  qqline(r1,col=2)
  acf(r1, ci.type="ma",60) # Prueba ACF (autocorrelación)
}
