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

pplot=function (model, xvar, by = NULL, breaks = NULL, partial = FALSE,
          type = c("conditional", "contrast"), scale = c("linear",
        "response"), leg.name = NULL, leg.labs = NULL, cols = c("red",
        "blue", "green", "purple"), linesize = 1.5, transp = 0.5,
          rsize = 2, rcolor = "red", rcols = FALSE, rshapes = TRUE)
}
\arguments{
  \item{model}{ A fitted model.}
  \item{xvar}{ Against which predictor to plot.}
  \item{by}{ Used for interactions. Plot are shown for levels of by. Continuous variables are changed to factors}
  \item{breaks}{ If a single number, number of levels. If 2 or more numbers, discrete values/levels of by.}
  \item{partial}{ Logical. Should residuals been shown?}
  \item{type}{Use 'contrast' for models with random factors}
  \item{scale}{ Linear or response}
  \item{leg.name}{ Optional. Name of legend.}
  \item{leg.labs}{ Optional. Legend labels.}
  \item{cols}{ Colors of confidence bands.}
  \item{linesize}{ Number. Set thickness of regression line?}
  \item{transp}{ Transparency of cols (0-1).}
  \item{rsize}{ Size of symbols for residuals.}
  \item{rcolor}{Color of residuals.}
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

\examples{

require(visreg)
require(ggplot2)
devAskNewPage(ask = FALSE)

fit <- lm(Ozone ~ Solar.R + Wind * Temp, data=airquality)

theme_set(theme_rufus(base_size=20))

pplot(fit,xvar="Wind",by="Temp", breaks=c(60,90),partial=TRUE,
      leg.name="Temperature",leg.labs=c("cool","warm"))

pplot(fit,xvar="Wind",by="Temp", breaks=c(3),partial=TRUE,
      leg.name="Temperature",leg.labs=c("cool","average","warm"),
      cols=c("yellow","orange","red"),rsize=3,rshapes=TRUE)

pplot(fit,xvar="Wind",by="Temp", breaks=c(3),partial=FALSE,
      leg.name="Temperature",leg.labs=c("cool","average","warm"),
      cols=c("yellow","orange","red"),rsize=3)+
      theme(legend.position = c(0.8, 0.8))

require(mgcv)
require (nlme)

mod.lme=lme(distance~age*Sex, random=~1|Subject, data=Orthodont)
pplot(mod.lme,xvar="age",by="Sex",partial=TRUE,leg.name="Gender")
# lme and lmer do no generate 95% confidence intervals

mod.bam=bam(distance~age*Sex+s(Subject, bs = 're'), data=Orthodont)
pplot(mod.bam,xvar="age",by="Sex")
pplot(mod.bam,xvar="age",by="Sex",partial=TRUE,leg.name="Gender")
# bam can be used to compute LINEAR mixed effects and generates confidence intervals.
# Coefficients, t and p-values are virtually identical to lme and lmer.

# pplot handles simple gams too.
fit <- gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp), data=airquality)
pplot(fit,xvar="Wind",partial=TRUE)

}

\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")

