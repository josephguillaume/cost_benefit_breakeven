\name{bivariate.breakeven}
\alias{bivariate.breakeven}
\title{
  Bivariate breakeven
}
\description{
  Calculate breakeven point by varying one factor, keeping all others at
  nominal value
}
\usage{
bivariate.breakeven(ranges, scen, baseline, n = 100)
}
\arguments{
  \item{ranges}{
    data.frame with only two rows and at least columns Variable, Min, Max.
}
  \item{scen}{
    Name of scenario accepted by \code{\link{NPV}}
}
\item{baseline}{
    Name of scenario accepted by \code{\link{NPV}}
}
  \item{n}{
    Number of points of first variable at which to evaluate second variable
}
}
\details{
  Breakeven for the pair of variables specified by \code{ranges} is
  calculated by solving for the first variable in \code{NPV(scen)-NPV(baseline)=0} using \code{\link{uniroot}} between the values
  specified by \code{Min} and \code{Max} of \code{ranges} for \code{n}
  values of the second variable.
}
\value{
  A n x 2 matrix with breakeven values of first and second
  variable. Columns are named by variable
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Joseph Guillaume
}
\seealso{
\code{\link{runShiny}} for web interface
}
\examples{
load(ranges)
bivariate.breakeven(ranges, "basin", "base", n = 10)
}
