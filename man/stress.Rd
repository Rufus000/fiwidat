\name{stress}
\alias{stress}
\docType{data}
\title{Stress Hormones
%%   ~~ data name/kind ... ~~
}
\description{
Fecal stress hormones in female wild boars as a function of age and reproductive state.
Note that females ar classified as juveniles in their first year of life, although they can reproduce.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("stress")}
\format{
  A data frame with 60 observations on the following 3 variables.
  \describe{
    \item{\code{alter}}{Age class: juvenile, subadult, or adult.}
    \item{\code{status}}{lactating or not.}
    \item{\code{stress}}{Fecal stress hormone level.}
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
Further reading:
Ahmad, Ejaz, Joe E. Brooks, Iftikhar Hussain, and M. Hafiz Khan. "Reproduction in Eurasian wild boar in central Punjab, Pakistan." Acta Theriologica 40, no. 2 (1995): 163-173.
}
\examples{
data(stress)
}
\keyword{datasets}
