\name{SimMix}
\alias{SimMix}
\docType{data}
\title{ Example 2-class data }
\description{
  This simulated data contains to groups. The item difficulties for the two groups are reversed. The analysis should make a clear example for those people less familiar with mixture models.
}
\usage{data(SimMix)}
\format{
  A data frame with 1000 observations on the following 10 variables.
  \describe{
    \item{\code{i1}}{a numeric vector}
    \item{\code{i2}}{a numeric vector}
    \item{\code{i3}}{a numeric vector}
    \item{\code{i4}}{a numeric vector}
    \item{\code{i5}}{a numeric vector}
    \item{\code{i6}}{a numeric vector}
    \item{\code{i7}}{a numeric vector}
    \item{\code{i8}}{a numeric vector}
    \item{\code{i9}}{a numeric vector}
    \item{\code{i10}}{a numeric vector}
  }
}
\source{
  Simulated to follow a 2-class mixture Rasch model.
}
\seealso{ mixRasch }
\examples{
data(SimMix)
}
\keyword{datasets}
