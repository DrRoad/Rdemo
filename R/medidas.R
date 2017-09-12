medidas <- function(x){
  M <- c()
  
  M <-summary(x)
  M <- c(M,kurtosis(x))
  names(M)[length(M)]='kurtosis'
  
  M <- c(M,skewness(x))
  names(M)[length(M)]='skewness'
  
  return(M)
}
