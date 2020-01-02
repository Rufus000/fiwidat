\name{fertil}
\alias{fertil}
\docType{data}
\title{
Crop Yield
%%   ~~ data name/kind ... ~~
}
\description{
The effects of different fertilizers on crop yields
}
\usage{data("fertil")}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{FERTIL}}{Fertilizer, factor with three levels}
    \item{\code{YIELD}}{Crop yield}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
T. Ruf simulated data
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fertil)
str(fertil)
plot(fertil)
}
\keyword{datasets}
