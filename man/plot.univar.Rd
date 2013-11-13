\name{plot.univar}
\alias{plot.univar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.univar(ranges, variable, scen, baseline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ranges}{
%%     ~~Describe \code{ranges} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{scen}{
%%     ~~Describe \code{scen} here~~
}
  \item{baseline}{
%%     ~~Describe \code{baseline} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ranges, variable, scen, baseline) 
{
    ranges0 <- subset(ranges, Variable == variable)
    basin_base0 <- createFun(scen, baseline, ranges0)
    plot(Vectorize(basin_base0), from = ranges0$Min, to = ranges0$Max, 
        main = variable, xlab = variable, ylab = "NPV")
    abline(v = ranges0$Modeled)
    abline(h = 0, lty = 2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
