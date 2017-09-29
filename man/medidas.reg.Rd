\name{medidas.reg}
\alias{medidas.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Recibe

datos,                  #Columna de datos de la serie de tiempo
  fechas,                 #Columna de fechas de la serie de tiempo
  frequency,              #Frecuencua atribuida a la serie de tiempo
  m,                      #Cantidad de periodos a pronosticar
  ano.inicio,             #ano inicio estimacion
  periodo.inicio,         #Periodo inicio estimacion
  number.model            #Numero modelo a seleccionar

Para calcular el comportamiento de la serie de tiempo dentro 6 modelos pre establecidos: Lineal, Cuadrático,Cúbico y a su vez estos, cada uno con variables indicadoras

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
medidas.reg(
  datos,
  fechas,
  frequency,
  m,
  ano.inicio,
  periodo.inicio,
  number.model)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{Columna de datos de la serie de tiempo}
  \item{fechas}{Columna de fechas de la serie de tiempo}
  \item{frequency}{Frecuencia atribuida a la serie de tiempo}
  \item{m}{Cantidad de periodos a pronosticar}
  \item{ano.inicio}{ano inicio estimacion}
  \item{periodo.inicio}{Periodo inicio estimacion}
  \item{number.model}{Numero modelo a seleccionar}
%%     ~~Describe \code{x} here~~
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%%  ~~who you are~~
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
