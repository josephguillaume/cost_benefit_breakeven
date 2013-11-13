\name{univariate.breakeven}
\alias{univariate.breakeven}
\title{
  Univariate breakeven
}
\description{
  Calculate breakeven point by varying one factor, keeping all others at
  nominal value
}
\usage{
univariate.breakeven(ranges, scen, baseline)
}
\arguments{
  \item{ranges}{
    data.frame with at least columns Variable, Min, Max.
}
  \item{scen}{
    Name of scenario accepted by \code{\link{NPV}}
}
\item{baseline}{
  Name of scenario accepted by \code{\link{NPV}}
}
}
\details{
  Breakeven for each variable in \code{ranges} is calculated by solving
  \code{scen-baseline=0} using \code{\link{uniroot}} between the values
  specified by \code{Min} and \code{Max} of \code{ranges}
}
\value{
  data.frame of ranges with additional column \code{break}
}
\author{
Joseph Guillaume
}
\seealso{
\code{\link{runShiny}} for web interface
}
\examples{
load(ranges)
univariate.breakeven(ranges,"basin","base")
}