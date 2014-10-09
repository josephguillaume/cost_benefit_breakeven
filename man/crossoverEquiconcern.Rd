\name{crossoverEquiconcern}
\alias{crossoverEquiconcern}
\title{
  Multivariate cross-over by equiconcern
}
\description{
  Identify crossover point of greatest concern by following lines of equiconcern
}
\usage{
crossoverEquiconcern(scen, baseline, ranges)
}
\arguments{
  \item{scen}{
    Name of scenario accepted by \code{\link{NPV}}
}
\item{baseline}{
  Name of scenario accepted by \code{\link{NPV}}
}
\item{ranges}{
  data.frame with at least columns Variable, Modeled, Min, Max.
}
}
\details{
  Assumes that cross-over point of greatest concern is on lines of
  equiconcern (where all variables have same level of concern). We
  therefore solve the equation \code{0=dNPV(loc)}, where
  \code{loc=abs(x-bound)/abs(modeled-bound)}. There are 2^n such lines,
  where n is the number of variables, so computational time increases exponentially.
}
\value{
  List with elements describing cross-over point identified
  \item{loc}{Level of concern [0,100]. At loc=0, we are at the bound. At loc=100, we are at the best guess}
  \item{bound}{Named vector of closest bound to cross-over point on each variable}
  \item{dNPV}{Difference in NPV at cross-over point (should be close to
    zero)}
  \item{values}{Named vector of values of cross-over point for variables
    in \code{ranges}}
}
\author{
  Joseph Guillaume
}
\examples{
data(ranges)
crossoverEquiconcern("basin","base",ranges[1:3,])
}
