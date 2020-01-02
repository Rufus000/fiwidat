\name{hasentod}
\alias{hasentod}
\docType{data}
\title{
Suvival Of Hares
%%   ~~ data name/kind ... ~~
}
\description{
Effects of food quality and reproduction on female hares
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("hasentod")}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{tot}}{Number of females dead before their 4th year of life.}
    \item{\code{lebend}}{Number of females alive in their 4th year of life.}
    \item{\code{junge}}{Number of young raised by female.}
    \item{\code{futter}}{A 2 level factor coding for food quality.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
T. Ruf simulated
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Modeled upon the "budworm" data in Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Springer.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Hasentod)
}
\keyword{datasets}
