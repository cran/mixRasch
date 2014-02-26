\name{rICC}
\alias{rICC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for producing theoretical and empirical item characteristic curves. }
\description{
  This function produces item characteristic curves for the family of Rasch models.
}

\usage{
rICC(delt, theta, itemVector, xlim, ylim, plotTitle, xlab, ylab, 
     col = c("black","white"), colTheme, expectedScore=FALSE, 
     empICC=FALSE, empOnly=FALSE, gDevice, file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delt}{ The threshold difficulties from a dichotomous or polytomous Rasch model. }
  \item{theta}{ The estimated theta values associated with the analysis and paired with itemVector, only required when requesting an empirical ICC. }
  \item{itemVector}{ The observed item responses, only required when requesting an empirical ICC. }
  \item{xlim}{ A vector overriding default limits for the x axis. }
  \item{ylim}{ A vector overriding default limits for the y axis. }
  \item{plotTitle}{ Controls the main plot title.}
  \item{xlab}{ The label for the x axis. }
  \item{ylab}{ The label for the y axis. }
  \item{col}{ A vector of the colors to be used in the plot. The first color will be used for item labels. The second color will be used for shading the area of rejection. }
  \item{colTheme}{ Four color themes ("cavaliers", "dukes", "spartans", "greys") are provided. If you provide a color theme, it will override the col paramater. }
  \item{expectedScore}{Flag indicating whether polytomous items are presented as category probabilities (FALSE) or expected item scores (TRUE). Will be set to TRUE when requesting empirical ICC. }
  \item{empICC}{ Flag indicating whether an empirical ICC should also be produced. }
  \item{empOnly}{ A flag indicating if ONLY the empirical ICC should be produced. }
  \item{gDevice}{ Controls graphics device. Options are "screen" (default), "jpg", or "png". }
  \item{file}{ The name of the output file if a device other than "screen" is chosen. }
  \item{...}{ Additional parameters passed to the plot command.}
}
\details{ The function produces an item characterisic curce plots. Both empirical and theoretical ICCs can be produced.
}

\author{ John T. Willse }

\examples{

library(mixRasch)
# Example data included with mixRasch
data(exRasch)

rasch1 <- mixRasch(exRasch,1,50, conv.crit=.0001, n.c=1)

# ICC for item 1
rICC(rasch1$item.par$delta[,1], rasch1$person.par$theta, 
     exRasch[,1], empICC=TRUE, colTheme="cavaliers")
}
\keyword{ misc }
