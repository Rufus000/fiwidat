\name{pplot}
\alias{pplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Regression Terms
%%  ~~function to do ... ~~
}
\description{
Plots regression terms against their predictors with partial residuals added.
}
\usage{
pplot(model, term, confint)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ A fitted model}
  \item{term}{ Which term to plot. Defaults to NULL, in wich case all terms are shown}
  \item{confint}{ Logical. Should confidence inervals be plotted?}
}
\details{
Wrapper for effects plots from package 'effects'
}
\value{

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
T. Ruf
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


model=lm(weight~ Time+Diet, data=ChickWeight)
pplot(model,"Time")
pplot(model, confint=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")

