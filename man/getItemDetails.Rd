\name{getItemDetails}
\alias{getItemDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convenience function returning information about an individual item. }
\description{
  This function extracts information about an individual item estimated using the mixRasch procedure.
}
\usage{
getItemDetails(raschResult, item, class=1, camelCase=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raschResult}{ The result of a mixRasch analysis. }
  \item{item}{ Either an integer representing the item position within the analysis or the item name (the column name from the dataframe used in the mixRasch analysis). }
  \item{class}{ The class in a mixture Rasch analysis for which you want the item stats. In a standard Rasch analysis, class will always be 1.}
  \item{camelCase}{ If TRUE, the variables returned are renamed to conform to camelCase style naming conventions. }
  }
\details{ The function only requires a mixRasch result and an item number or name. If the analysis is a mixture analysis, the class from which the result is to be taken must be provided. The camelCase option is available so results can be exported to other software that might not supprt some types of R names (e.g., names with "."s in them).
}
\value{
item.name=item, n.cat=n.cat+1, delta.i = rR$delta.i[itemSelect], SE.delta.i = rR$SE.delta.i[itemSelect], tau = rR$tau[1:n.cat,itemSelect], SE.tau = SE.tau, infit = rR$in.out[,"infit"][itemSelect], in.Z = rR$in.out[,"in.Z"][itemSelect],outfit = rR$in.out[,"outfit"][itemSelect],out.Z = rR$in.out[,"out.Z"][itemSelect])  
  
  \item{item.name }{The name of the item.}
  \item{n.cat }{The number categories (e.g., 2 for dichotomous). }
  \item{delta.i }{Average step difficulty.}
  \item{SE.delta.i }{Standard error for delta.i.}
  \item{tau }{Step difficulty as a deviation from delta.i.}
  \item{SE.tau }{Standard error for tau.}
  \item{infit}{The infit statistic for the item}
  \item{in.Z}{The standardized infit statistic for the item}
  \item{outfit}{The outfit statistic for the item}
  \item{out.Z}{The standardized outfit statistic for the item}
}
\author{ John T. Willse }

\examples{

# Example data included with mixRasch
data(SimMix)

test1 <- mixRasch(SimMix,1,50, conv.crit=.0001, n.c=1)
getItemDetails(test1,2)

}
\keyword{ misc }
