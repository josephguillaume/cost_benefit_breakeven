\name{cost_benefit_breakeven-package}
\alias{cost_benefit_breakeven-package}
\alias{cost_benefit_breakeven}
\docType{package}
\title{
Exploration of breakeven points for cost-benefit analysis
}
\description{
Interactive interface to identify univariate, bivariate and multivariate breakeven points}
\details{
\tabular{ll}{
Package: \tab cost_benefit_breakeven\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-11-14\cr
License: \tab GPL-3\cr
}

Load a data.frame specifying ranges of values to explore (See \code{\link{ranges}})
Use \code{runShiny()} to show interface.

Alternatively, use \code{\link{univariate.breakeven}}, \code{\link{bivariate.breakeven}} directly.
}
\author{
Joseph Guillaume <joseph.guillaume@gmail.com>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
\code{\link{runShiny}}
}
\examples{
data(ranges)
runShiny()
}
