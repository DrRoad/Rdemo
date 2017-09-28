library(forecast)

medidas.reg <- function(
  datos,
  fechas,
  frequency,
  m,
  año.inicio,
  periodo.inicio,
  number.model
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
    k <- 2
  } else if (number.model == 2){
    model <- lm(yi ~ ti + ti2)
    k <- 3
  } else if (number.model == 3){
    model <- lm(yi ~ ti + ti2 + ti3)
    k <- 4
  } else if (number.model == 4) {
    model <- lm(yi ~ ti + It)
    k <- 2 + (frequency - 1)
  } else if (number.model == 5){
    model <- lm(yi ~ ti + ti2 + It)
    k <- 3 + (frequency - 1)
  } else {
    model <- lm(yi ~ ti + ti2 + ti3 + It)
    k <- 4 + (frequency - 1)
  }

  y.est <- model$fitted.values
  sse = sum((y.est-yi)^2)
  ssr = sum((yi-mean(yi))^2)
  mse = sse/(n-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (n-1)*(1-R2)/(n-k)
  aic = log((n-k)*exp(2*k/n)*mse/n)
  bic = log(n^(k/n)*(n-k)*mse/n)
  Valor = c(Ra2, mse, aic, bic)
  names(Valor) = c("R2-adjusted","MSE","logAIC","logBIC")
  return(data.frame(Valor))
}
