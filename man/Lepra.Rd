\name{Lepra}
\alias{Lepra}
\docType{data}
\title{
Leprosy Data
%%   ~~ data name/kind ... ~~
}
\description{
Effects of different treatments on Mycobacterium leprae load.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Lepra")}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{TREATMT}}{A factor with 3 levels coding for treatments. Treatment C is a placebo.}
    \item{\code{BACBEF}}{Pathogen load before the treatment.}
    \item{\code{BACAFTER}}{Pathogen load after the treatment.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://biosun1.harvard.edu/~fitzmaur/ala
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{

Snedecor GW, Cochran WG (1967) Statistical Methods, (6th edn). Ames, Iowa: Iowa State University Press
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Lepra)
}
\keyword{datasets}
