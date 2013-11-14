\name{plot.univar}
\alias{plot.univar}
\title{
  Plot variation of difference in NPV for single variable
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
Joseph Guillaume
}
\examples{
data(ranges)
plot.univar(ranges,"surface.evap.rate","basin","base")
}