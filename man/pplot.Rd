\name{pplot}
\alias{pplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Regression Terms using ggplot2
%%  ~~function to do ... ~~
}
\description{
Plots regression terms against their predictors.
Generates ggplots based on library visreg
}
\usage{
pplot (model,xvar,by=NULL,breaks=NULL, partial=FAlSE,
      leg.name=NULL,leg.labs=NULL,cols=c("red","blue","green","purple"),
      transp=0.5, rsize=2,rcols=FALSE, rshapes=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ A fitted model.}
  \item{xvar}{ Against which predictor to plot.}
  \item{by}{ Used for interactions. Plot are shown for levels of by. Continuous variables are changed to factors}
  \item{breaks}{ If a single number, number of levels. If 2 or more numbers, discrete values/levels of by.}
  \item{partial}{ Logical. Should residuals been shown?}
  \item{leg.name}{ Optional. Name of legend.}
  \item{leg.labs}{ Optional. Legend labels.}
  \item{cols}{ Colors of confidence bands.}
  \item{transp}{ Transparency of cols (0-1).}
  \item{rsize}{ Size of symbols for residuals.}
  \item{rcols}{ Logical. Should residuals been shown in different shades of gray per level of by?}
  \item{rshapes}{ Logical. Should residuals been shown in different shapeas per level of by?}
}
\details{
Wrapper for essentials of visreg. See ?visreg for more.
}
\value{

}
\references{
Breheny P and Burchett W (2017). Visualization of Regression Models Using visreg. The R Journal, 9: 56-71.
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

require(visreg)
require(ggplot2)

fit <- lm(Ozone ~ Solar.R + Wind * Temp, data=airquality)

theme_set(theme_rufus(base_size=20))

pplot(fit,xvar="Wind",by="Temp", breaks=c(60,90),partial=TRUE,
      leg.name="Temperature",leg.labs=c("cool","warm"))

pplot(fit,xvar="Wind",by="Temp", breaks=c(3),partial=TRUE,
      leg.name="Temperature",leg.labs=c("cool","average","warm"),
      cols=c("yellow","orange","red"),rcols=TRUE)

pplot(fit,xvar="Wind",by="Temp", breaks=c(3),partial=TRUE,
      leg.name="Temperature",leg.labs=c("cool","average","warm"),
      cols=c("yellow","orange","red"),rsize=3,rshapes=TRUE)

pplot(fit,xvar="Wind",by="Temp", breaks=c(3),partial=FALSE,
      leg.name="Temperature",leg.labs=c("cool","average","warm"),
      cols=c("yellow","orange","red"),rsize=3)+
      theme(legend.position = c(0.8, 0.8))

pplot(fit,xvar="Solar.R",partial=TRUE) +
  ylim(0,100)




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")

