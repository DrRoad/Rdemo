library(dataseries)
library(forecast)
library(ggplot2)

residuals.reg <- function(
  datos,
  fechas,
  frequency,
  m,
  ano.inicio,
  periodo.inicio,
  number.model
){

  y <- ts(data = datos,frequency = frequency, start = c(ano.inicio, periodo.inicio))

  n <- length(y)

  yi <- ts(y[1:(n-m)],frequency=frequency)

  ti <- seq(1:length(yi))
  ti2 <- ti*ti
  ti3 <- ti*ti*ti
  It <- seasonaldummy(yi)

  if (number.model == 1){
    model <- lm(yi ~ ti)
    r <- model$residuals
  } else if (number.model == 2){
    model <- lm(yi ~ ti + ti2)
    r <- model$residuals
  } else if (number.model == 3){
    model <- lm(yi ~ ti + ti2 + ti3)
    r <- model$residuals
  } else if (number.model == 4) {
    model <- lm(yi ~ ti + It)
    r <- model$residuals
  } else if (number.model == 5){
    model <- lm(yi ~ ti + ti2 + It)
    r <- model$residuals
  } else {
    model <- lm(yi ~ ti + ti2 + ti3 + It)
    r <- model$residuals
  }

    d <- data.frame(ti, r)
    p <- ggplot(d, aes(ti,r)) +
      geom_point() +
      ggtitle("Residuals") +
      xlab("ti") +
      ylab("Residuals")
    p <- p +geom_line(yintercept=0)+geom_smooth()
    return(p)
}

