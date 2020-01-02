\name{harecycle}
\alias{harecycle}
\docType{data}
\title{
Holley's Hare Activity Data
%%   ~~ data name/kind ... ~~
}
\description{
Seasonal cycle of activity onset in free-living brown hares
}
\usage{data("harecycle")}
\format{
  A data frame with 44 observations on the following 2 variables.
  \describe{
    \item{\code{week}}{Week of the year.}
    \item{\code{odexits}}{Time of activity onset. Negative values are hours before sunset, positive values are hours after sunset.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Holley, A. J. (1992). Studies on the biology of the brown hare (Lepus europaeus) with particular reference to behaviour (Doctoral dissertation, Durham University).
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(harecycle)
plot(harecycle)
}
\keyword{datasets}
