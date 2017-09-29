\name{medidas}
\alias{medidas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Arroja los estadisticos básicos de la columna de datos ingresada}
%%  ~~function to do ... ~~
}
\description{La función recibe

x<-c()

Como datos, que posteriormente son usados para el calculo de estadisticos: Kurtosis, Skweness y Sd.

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
\value{kurtosis {x}
skweness {x}
sd {x}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ David Rivera <dfriveraol@unal.edu.co>
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

x<-c(4,6,8,10,0,-10,6,0)

medidas(x)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{kurtosis}% use one of  RShowDoc("KEYWORDS")
\keyword{skewness}% __ONLY ONE__ keyword per line
\keyword{sd}% __ONLY ONE__ keyword per line
