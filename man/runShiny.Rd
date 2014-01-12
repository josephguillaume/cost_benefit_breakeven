\name{runShiny}
\alias{runShiny}
\title{
Run Shiny Web interface
}
\description{
Web interface for univariate, bivariate and multivariate breakeven analysis
}
\usage{
runShiny(...,name="shiny_perctolimit")
}
\arguments{
  \item{\dots}{
    Arguments to pass to \code{\link{runApp}}
}
  \item{\name}{
    User interface to use: \code{shiny1} or \code{shiny_perctolimit} (default)
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
