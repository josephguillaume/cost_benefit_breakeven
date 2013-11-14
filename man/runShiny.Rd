\name{runShiny}
\alias{runShiny}
\title{
Run Shiny Web interface
}
\description{
Web interface for univariate, bivariate and multivariate breakeven analysis
}
\usage{
runShiny(...)
}
\arguments{
  \item{\dots}{
    Arguments to pass to \code{\link{runApp}}
}
}
\details{
  Starts a webserver in R and a browser session
}
\value{
  None. Run for its side effect.
}
\author{
Joseph Guillaume
}
\examples{
data(ranges)
runShiny()
}
