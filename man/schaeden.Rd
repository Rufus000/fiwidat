\name{schaeden}
\alias{schaeden}
\docType{data}
\title{
Damages by Wild Boar
%%   ~~ data name/kind ... ~~
}
\description{
Factors affecting the number of claims for agricultural damages by wild boar in Austria.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("schaeden")}
\format{
  A data frame with 720 observations on the following 6 variables.
  \describe{
    \item{\code{Meldungen}}{Number of claims per district}
    \item{\code{Dichte}}{Factor coding for wild boar density in the district}
    \item{\code{Frucht}}{Factor coding for crop type damaged}
    \item{\code{Waldanteil}}{High vs. low proportion of woods}
    \item{\code{Waldtyp}}{Forest type}
    \item{\code{Besiedlung}}{Human population density}
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
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(schaeden)
}
\keyword{datasets}
