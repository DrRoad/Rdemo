library(dataseries)
library(forecast)
library(ggplot2)
library(gridExtra)

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
      geom_hline(yintercept=0, lty=3) +
      ggtitle("Residuals") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("ti") +
      ylab("Residuals")
    p <- p +geom_line(yintercept=0)+geom_smooth()


    s <- sqrt(deviance(model)/df.residual(model))
    rs <- r/s

    w <- qnorm(ppoints(rs))
    d2 <- data.frame(w, rs=sort(rs))
    p1 <- ggplot(d2, aes(w, rs)) +
      geom_point() +
      geom_line(aes(x=rs, y=rs), col='red') +
      ggtitle("Normal Q-Q") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles")


    p2 <- ggplot(model, aes(x=r)) +
      geom_density(fill="yellow", colour=NA, alpha=.2) +
      geom_line(stat = "density") +
      expand_limits(y = 0) +
      ggtitle("Density") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Residuals") +
      ylab("Density")


    ww <- acf(r, plot = FALSE)
    www <- with(ww, data.frame(lag, acf))
    significance_level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(r)))
    p3 <- ggplot(data = www, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_hline(yintercept=c(significance_level,-significance_level), lty=3, col='blue') +
      geom_segment(mapping = aes(xend = lag, yend = 0)) +
      ggtitle("ACF") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Lag") +
      ylab("ACF")

    return(grid.arrange(p,p1,p2,p3, ncol=2))
}
