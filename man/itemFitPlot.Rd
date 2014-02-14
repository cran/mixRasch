\name{itemFitPlot}
\alias{itemFitPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for producing item difficulty by fit statistic plots. }
\description{
  This function produces item difficulty and fit statistic plots for Rasch calibrated items estimated using the mixRasch procedure.
}

\usage{
itemFitPlot(raschResult, itemSet, useItemNames = TRUE,
            fitStat = "infit", plotTitle="Item Fit Plot",
            xlab, ylab, xlim, ylim, refLines, 
            col = c("black","white"), colTheme,
            gDevice, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raschResult}{ A mixRasch object, where n.c = 1 (i.e., no mixture models). }
  \item{itemSet}{ An optional vector of item names or item positions for plotting subsets of items. }
  \item{useItemNames}{ If TRUE, then item names are used. If false item numbers are used. }
  \item{fitStat}{The item fit statistic ("infit", "outfit", "in.Z", "out.Z") used in the plot. }
  \item{plotTitle}{ Controls the main plot title.}
  \item{xlab}{ The label for the x axis. }
  \item{ylab}{ The label for the y axis. }
  \item{xlim}{ A vector overriding default limits for the x axis. }
  \item{ylim}{ A vector overriding default limits for the y axis. }
  \item{refLines}{ A vector overriding the default lower and upper reference lines that define the region of rejection.}
  \item{col}{ A vector of the colors to be used in the plot. The first color will be used for item labels. The second color will be used for shading the area of rejection. }
  \item{colTheme}{ Four color themes ("cavaliers", "dukes", "spartans", "greys") are provided. If you provide a color theme, it will override the col paramater. }
  \item{gDevice}{ Controls graphics device. Options are "screen" (default), "jpg", or "png". }
  \item{file}{ The name of the output file if a device other than "screen" is chosen. }
}
\details{ The function produces an item difficulty by item fit plot. Items are plotted using either their names or their test position (see useItemNames). Lower and upper reference line are drawn (see refLines) that help identify misfitting items. For infit and outfit, default reference lines are drawn at .7 and 1.3. For in.Z and out.Z, default reference lines are drawn at -2 and +2. Using col or colTheme, the region beyond the reference line may be shaded.
}

\author{ John T. Willse }

\examples{

# Example data included with mixRasch
data(exRasch)

rasch1 <- mixRasch(exRasch,1,50, conv.crit=.0001, n.c=1)

# Default Fit Plot
itemFitPlot(rasch1)

# Using Outfit and a color theme
itemFitPlot(rasch1, fitStat="outfit", colTheme="spartans")

}
\keyword{ misc }
