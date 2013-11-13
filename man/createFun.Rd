\name{createFun}
\alias{createFun}
\title{
Create selection function for difference in NPV
}
\description{
Utility that creates a function that returns \code{NPV(scen)-NPV(baseline)}
for a given vector of parameters
}
\usage{
createFun(scen, baseline, ranges, fixed.vals = NULL)
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
  \item{fixed.vals}{
    named vector of values to fix, with names corresponding to arguments
    to NPV. Elements of vectors can be set with \code{$} operator, e.g. water.available$groundwater
}
}
\value{
A function that takes as argument a vector of values corresponding to
variables in ranges$Variable and returns \code{NPV(scen)-NPV(baseline)}
}
\author{
Joseph Guillaume
}
\examples{
load(ranges)
diff.NPV<-createFun("basin","base",ranges[1:2,])
diff.NPV(c(25,200))
}