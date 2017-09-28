df <- read.csv('Tour4.csv',header = TRUE, sep=',')

datos <- df[,ncol(df)]

fechas <- as.Date(df[,ncol(df)-1])
frequency <- 12
m <- 12
año.inicio <- 2005
periodo.inicio <- 1
number.model <- 1

plot.model(
  datos,
  fechas,
  frequency,
  m,
  año.inicio,
  periodo.inicio,
  number.model
)
