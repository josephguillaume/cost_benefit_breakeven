\name{getPom}
\alias{getPom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPom(ranges, start.pos, scen, baseline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ranges}{
%%     ~~Describe \code{ranges} here~~
}
  \item{start.pos}{
%%     ~~Describe \code{start.pos} here~~
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
function (ranges, start.pos, scen, baseline) 
{
    netdiff <- createFun(scen, baseline, ranges)
    maxmin = -sign(netdiff(start.pos))
    nn <- nsga2(function(x) abs(x - start.pos), idim = nrow(ranges), 
        odim = nrow(ranges), constraints = function(x) maxmin * 
            netdiff(x), cdim = 1, lower.bounds = ranges$Min, 
        upper.bounds = ranges$Max, generations = 200, popsize = 200, 
        mprob = 1/nrow(ranges))
    pom <- nn$par[nn$pareto.optimal, ]
    if (NROW(pom) > 0) 
        colnames(pom) <- ranges[, 1]
    pom
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
