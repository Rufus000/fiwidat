\name{aggregateFrame}
\alias{aggregateFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Summary Statistics of Data Subsets
}
\description{
Splits the data into subsets, computes summary statistics for each numerical variable, and returns the result in a convenient form.
}
\usage{
aggregateFrame(data, by, compute, na.rm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame}
  \item{by}{grouping elements,as long as the variables in the data frame data.}
  \item{compute}{A function to compute the summary statistics which can be applied to all numerical data subsets}
  \item{na.rm}{logical. Should missing values be removed}
}
\details{
For non-numerical variables, the function returns the alphabetically smallest value.
}
\value{
An aggregated data frame.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
T. Ruf
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
aggregate
}
\examples{
data(CO2)
aggregateFrame(CO2,CO2$Type,compute="median", na.rm=T)


comb=paste(CO2$Plant, CO2$Type)
aggregateFrame(CO2,comb,compute="min", na.rm=F)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")

