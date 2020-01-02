\name{titanic}
\alias{titanic}
\docType{data}
\title{
Survival of passengers on the Titanic
}
\description{
This data set provides information on the fate of passengers on the fatal maiden voyage of the ocean liner ‘Titanic’, summarized according to economic status (class), sex, age and survival.
}
\usage{data("titanic")}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{NoSurv}}{Number of passengers that did not survive.}
    \item{\code{YesSurv}}{Number of survivors.}
    \item{\code{Class}}{1st, 2nd, 3rd, Crew}
    \item{\code{Sex}}{Male, Female}
    \item{\code{Age}}{Child, Adult}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
rearranged from the R base dataset Titanic

Dawson, Robert J. MacG. (1995), The ‘Unusual Episode’ Data Revisited. Journal of Statistics Education, 3. doi: 10.1080/10691898.1995.11910499.

The source provides a data set recording class, sex, age, and survival status for each person on board of the Titanic, and is based on data originally collected by the British Board of Trade and reprinted in:

British Board of Trade (1990), Report on the Loss of the ‘Titanic’ (S.S.). British Board of Trade Inquiry Report (reprint). Gloucester, UK: Allan Sutton Publishing.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(titanic)
}
\keyword{datasets}
