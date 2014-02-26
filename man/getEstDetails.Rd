\name{getEstDetails}
\alias{getEstDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convenience function returning information about the estimation. }
\description{
  This function extracts information about the estimation procedure conducted using the mixRasch procedure.
}
\usage{
getEstDetails(raschResult, camelCase=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raschResult}{ The result of a mixRasch analysis. }
  \item{camelCase}{ If TRUE, the variables returned are renamed to conform to camelCase style naming conventions. }
  }
\details{ The function only requires a mixRasch result. The camelCase option is available so results can be exported to other software that might not supprt some types of R names (e.g., names with "."s in them).
}
\value{
  \item{model }{Reports which model was estimated.}
  \item{n.c }{Number of latent classes.}
  \item{iter }{The number of completed iterations.}
  \item{max.change }{The largest model parameter change in each class in the last iteration of the estimation.}
  \item{converge.flag }{Indicates if convergence was reached.}
  \item{run.time}{Reports time elapsed during estimation.}
}
\author{ John T. Willse }

\examples{

# Example data included with mixRasch
data(SimMix)

test1 <- mixRasch(SimMix,1,50, conv.crit=.0001, n.c=1)
getEstDetails(test1)

}
\keyword{ misc }
