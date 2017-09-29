\name{medidas}
\alias{medidas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Arroja los estadisticos básicos de la columna de datos ingresada}
%%  ~~function to do ... ~~
}
\description{Inicialmente la función contiene una agrupación que inicializa vacía para posteriormente recibir información que esté arrojada en una columna de datos, posteriormente la función permite arrojar 3 estadísticos de análisis (Kurtosis, Skweness y Sd)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
medidas(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A column of Data}
%%     ~~Describe \code{x} here~~
}
}
\details{M es una agrupación de datos que inicializa vacío y después toma los valores de los estadísticos de x
%%  ~~ If necessary, more details than the description above ~~
}
\value{Arroja la curtosis (grado de concentración) de los valores incluidos en la columna de datos
Arroja la desviación estándar de los valores incluidos en la columna de datos
Arroja la asimetría de los valores incluidos en la columna de datos
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
medidas <- function(x){
  M <- c()

  M <-summary(x)
  M <- c(M,kurtosis(x))
  names(M)[length(M)]='kurtosis'
  return (M)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{kurtosis}% use one of  RShowDoc("KEYWORDS")
\keyword{skewness}% __ONLY ONE__ keyword per line
\keyword{sd}% __ONLY ONE__ keyword per line
