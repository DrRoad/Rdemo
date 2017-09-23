medidas <- function(x){
  M <- c()

  M <-summary(x)
  M <- c(M,kurtosis(x))
  names(M)[length(M)]='kurtosis'

  M <- c(M,skewness(x))
  names(M)[length(M)]='skewness'

  M <- c(M,sd(x))
  names(M)[length(M)]='Stand. Desv.'

  return(M)
}

#por aqui paso maria
