\name{heartrates}
\alias{heartrates}
\docType{data}
\title{
Red Deer Heartrates
%%   ~~ data name/kind ... ~~

}
\description{
Weekly means of resting heart rates from 24 red deer over 12 weeks in winter (Jan-Mar)
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("heartrates")}
\format{
  A data frame with 288 observations on the following 4 variables.
  \describe{
    \item{\code{heartrate}}{Weekly mean resting heart rarte (bpm).}
    \item{\code{Animal}}{Animal id.}
    \item{\code{week}}{Observation week}
    \item{\code{Diet}}{In addition to natural forage, they animals received high or low protein pellets.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
W. Arnold & T. Ruf, unpublished
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
For an anlysis of an extended data set see Arnold et al. (2004) Am J Physiol Regul Integr Comp Physiol 286: R174–R181
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(heartrates)
}
\keyword{datasets}
