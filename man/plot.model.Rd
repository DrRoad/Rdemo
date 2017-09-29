\name{plot.model}
\alias{plot.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graficación del Modelo de Regresion
}
\description{
Entrada de

datos,                  #Columna de datos de la serie de tiempo
  fechas,                 #Columna de fechas de la serie de tiempo
  frequency,              #Frecuencua atribuida a la serie de tiempo
  m,                      #Cantidad de periodos a pronosticar
  ano.inicio,             #ano inicio estimacion
  periodo.inicio,         #Periodo inicio estimacion
  number.model            #Numero modelo a seleccionar

  para graficar la serie de tiempo de datos observados, datos ajustados, datos pronosticados sobre un elemento de ggplot
}
\usage{
plot.model(
datos,                  #Columna de datos de la serie de tiempo
  fechas,                 #Columna de fechas de la serie de tiempo
  frequency,              #Frecuencua atribuida a la serie de tiempo
  m,                      #Cantidad de periodos a pronosticar
  ano.inicio,             #ano inicio estimacion
  periodo.inicio,         #Periodo inicio estimacion
  number.model            #Numero modelo a seleccionar
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{Columna de datos de la serie de tiempo}
  \item{fechas}{Columna de fechas de la serie de tiempo}
  \item{frequency}{Frecuencua atribuida a la serie de tiempo}
  \item{m}{Cantidad de periodos a pronosticar}
  \item{ano.inicio}{ano inicio estimacion}
  \item{periodo.inicio}{Periodo inicio estimacion}
  \item{number.model}{Numero modelo a seleccionar: 1: Lineal, 2: Cuadratico, 3: cubico, 4: Lineal con Indicadoras, 5:Cuadratica con Indicadoras, 6:Cubica con Indicadoras}


%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Grafica la serie de tiempo ajustada y pronosticada segun los parametros introducidos
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
David Rivera <dfriveraol@unal.edu.co>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
plot.model(
datos,                  #Columna de datos de la serie de tiempo
  fechas,                 #Columna de fechas de la serie de tiempo
  frequency,              #Frecuencua atribuida a la serie de tiempo
  m,                      #Cantidad de periodos a pronosticar
  ano.inicio,             #ano inicio estimacion
  periodo.inicio,         #Periodo inicio estimacion
  number.model            #Numero modelo a seleccionar
)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dataseries }% use one of  RShowDoc("KEYWORDS")
\keyword{forecast}% __ONLY ONE__ keyword per line
