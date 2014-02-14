\name{personItemPlot}
\alias{personItemPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for producing back-to-back histograms of item and person distributions. }
\description{
  This function will enable you to make back-to-back histograms of the item and person distributions estimated using the mixRasch procedure.
}

\usage{
personItemPlot(raschResult, nBreaks=15, plotTitle="Person Item Histogram",
           xlab = "Relative Frequency", ylab = "Ability", 
           col = c("darkgrey","lightgrey"), colTheme, makeLegend=TRUE, 
           legendLabels=c("items", "people"), legendLoc="bottomleft",
           gDevice, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raschResult}{ A mixRasch object, where n.c = 1 (i.e., no mixture models). }
  \item{nBreaks}{ The number of bins to be used in creating the histograms. }
  \item{plotTitle}{ The title to be used on the plot. }
  \item{xlab}{ The label for the x axis. }
  \item{ylab}{ The label for the y axis. }
  \item{col}{ A vector of the colors to be used in the plot. The first color will be used for items. The second color will be used for people. }
  \item{colTheme}{ Four color themes ("cavaliers", "dukes", "spartans", "greys") are provided. If you provide a color theme, it will override the col paramater. }
  \item{makeLegend}{ Controls whether a legend for the is colors used in the plot. }
  \item{legendLabels}{ A vector that allows for customized legend labels. }
  \item{legendLoc}{ Allows placement of the legend in different location. Uses the same discriptions as the standard legend function. }
  \item{gDevice}{ Controls graphics device. Options are "screen" (default), "jpg", or "png". }
  \item{file}{ The name of the output file if a device other than "screen" is chosen. }
}
\details{ The function produces a standard person and item back-to-back histogram.  These plots are only appropriate for standard Rasch models (i.e., not multiple-class mixture models). When used with polytomous model results, the item difficulty is based on the overall (average) step difficulty for each polytomous item.
}

\author{ John T. Willse }

\examples{

# Example data included with mixRasch
data(SimMix)

rasch1 <- mixRasch(SimMix,1,50, conv.crit=.0001, n.c=1)

personItemPlot(rasch1)

}
\keyword{ misc }
