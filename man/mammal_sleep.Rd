\name{mammal_sleep}
\alias{mammal_sleep}
\docType{data}
\title{Mammalian sleep
%%   ~~ data name/kind ... ~~
}
\description{
Dataset from Allison and Cicchetti (1976) of 62 mammal species on the interrelationship between sleep, ecological, and constitutional variables. The dataset contains missing values on five variables.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("mammal_sleep")}
\format{
  A data frame with 62 observations on the following 11 variables.
  \describe{
    \item{\code{species}}{Mammal species.}
    \item{\code{bodyweight}}{in kg.}
    \item{\code{brainweight}}{in g.}
    \item{\code{sws}}{Duration of slow wave sleep (h/d).}
    \item{\code{rem}}{Duration of rem sleep (h/d).}
    \item{\code{totalsleep}}{Duration of total sleep (h/d).}
    \item{\code{longevity}}{Maximum life span (years).}
    \item{\code{gestationtime}}{Duration of gestation (d).}
    \item{\code{predation}}{Predation risk index (1-5), 1 = least likely to be preyed upon.}
    \item{\code{sleepexposure}}{Sleep exposure index (1-5), 1 = least exposed (e.g. animal sleeps in a well-protected den), 5 = most exposed.

}
    \item{\code{danger}}{
Overall danger index (1-5) based on the above two indices and other information, 1 = least danger (from other animals), 5 = most danger (from other animals)}
  }
}
\details{
Allison and Cicchetti (1976) investigated the interrelationship between sleep, ecological, and constitutional variables. They assessed these variables for 39 mammalian species. The authors concluded that slow-wave sleep is negatively associated with a factor related to body size. This suggests that large amounts of this sleep phase are disadvantageous in large species. Also, paradoxical sleep (REM sleep) was associated with a factor related to predatory danger, suggesting that large amounts of this sleep phase are disadvantageous in prey species.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
library 'mice'
Stef van Buuren, Karin Groothuis-Oudshoorn (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. URL  https://www.jstatsoft.org/v45/i03/.

}
\references{
Allison, T., Cicchetti, D.V. (1976). Sleep in Mammals: Ecological and Constitutional Correlates. Science, 194(4266), 732-734.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mammal_sleep)
}
\keyword{datasets}
