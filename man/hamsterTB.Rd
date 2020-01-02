\name{hamsterTB}
\alias{hamsterTB}
\docType{data}
\title{Hamster Body Temperature
%%   ~~ data name/kind ... ~~
}
\description{
Hourly measurements of body temperature in Djungarian dwarf hamsters with missing values.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("hamsterTB")}
\format{
  A data frame with 240 observations on the following 2 variables.
  \describe{
    \item{\code{Time}}{hour}
    \item{\code{Tb}}{body temperature (°C)}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
T. Ruf unpublished data
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(hamsterTB)
plot(hamsterTB$Time, hamsterTB$Tb, type="l")
}
\keyword{datasets}
