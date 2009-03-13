\name{mixRasch}
\alias{mixRasch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for estimating and evaluating mixture Rasch models using JMLE }
\description{
  This function will estimate a mixture Rasch model using joint maximum likelihood estimation (JMLE). The estimation is based on a mixture partial credit model. Step parameters can be constrained to estimate a mixture rating scale model.
}
\usage{
mixRasch(data, steps, max.iter = 50, conv.crit = 0.001, model = "RSM", 
         n.c = 1, class, metric, info.fit = TRUE, treat.extreme = 0.3, 
         maxchange = 1.5, maxrange = c(-4, 4), as.LCA = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A rectangular data set (matrix or data frame) to be analyzed. }
  \item{steps}{ The maximum number of item thresholds to be estimated. Some items may have less than the maximum. }
  \item{max.iter}{ Maximum number EM iterations }
  \item{conv.crit}{ Estimation stops when the largest model parameter change is smaller than this criterion. }
  \item{model}{ "RSM" (the default) constrains all step parameters to be equal (i.e., estimates a rating scale model). 
                 Assumes all items have the same number of steps. "PCM" allows step parameters to differ across items (i.e., estimates a partial credit model). 
                 The number of steps can differ across items.  }
  \item{n.c}{ Number of latent classes. }
  \item{class}{ Optional matrix of starting values for latent class membership. }
  \item{metric}{ Not implemented. Will be an optional argument for setting the final scale of the Rasch results. }
  \item{info.fit}{ If "True" the information based criteria of fit (AIC, BIC) are estimated. }
  \item{treat.extreme}{ Adjustment to perfect response vectors to allow estimation of person parameters. Perfect vectors are not used during item parameter estimation. }
  \item{maxchange}{ Limits the change to model parameters in a single iteration. Helps keep estimates reasonable in the first few iterations. }
  \item{maxrange}{ Admissible range of item difficulties.  }
  \item{as.LCA}{ If TRUE, all person parameters are constrained to equal zero. That analysis accomplishes a latent class analysis rather than a mixture Rasch model. }
}
\details{ The analyzed data should be a rectangular data file. Missing data are permitted and handled in the usual JMLE manner. Large amounts of missing data will slow down estimation. Basic results on item parameters are returned to the screen. When more than one class is specified (n.c greater than 1) Rasch model parameters and measures of fit are returned in a list, with each class's results returned in a separate element of that list. When n.c = 1 a standard Rasch model analysis is performed.
}
\value{
  \item{LatentClass }{One element for each latent class. Each LatentClass element is itself a list containing within class results: item statistics (i.stat), person parameters and measures of person fit (person.par), item parameters and measures of item fit (item.par). }
  \item{max.change }{The largest model parameter change in each class in the last iteration of the estimation.}
  \item{class }{A rectangular matrix of the order number of people by number of classes. Values represent the probability that a person (row) belongs to a particular class (column).}
  \item{iter }{The number of completed iterations.}
  \item{converge.flag }{Indicates if convergence was reached.}
  \item{info.fit}{Provides (if requested) AIC, BIC, CAIC, log likelihood, number of estimated parameters, and number of persons used in the estimation.}
  \item{model }{Reports which model was estimated.}
  \item{removed.items}{Provides a vector indicating which, if any, items had to be removed from estimation for having perfect vectors.}
  \item{run.time}{Reports time elapsed during estimation.}
}
\references{ Willse, J. T. (2009). Mixture Rasch models with joint maximum likelihood estimation. Presented at the National Council on Measurement in Education annual meeting. San Diego. }
\author{ John T. Willse }
\note{   Be aware that this function is an early implementation of the procedure. Please contact the author if you encounter any bugs or if you have questions or suggestions. 
}

\examples{

# Example data included with mixRasch
data(SimMix)

test1 <- mixRasch(SimMix,1,50, conv.crit=.0001, n.c=1)
test2 <- mixRasch(SimMix,1,500, conv.crit=.0001, n.c=2)

# The 3 class solution may take a while to converge or need
# to be restarted if it fails to converge.
test3 <- mixRasch(SimMix,1,1000, conv.crit=.0001, n.c=3)

# Notice that the AIC and BIC are lowest for the 2 class solution
rbind(test1$info.fit,test2$info.fit,test3$info.fit)

# Notice that the two "difficulty" columns are ordered differently
# The results reflect that the two groups in the data set have a reversed
# scale from one another.
test2

}
\keyword{ misc }
