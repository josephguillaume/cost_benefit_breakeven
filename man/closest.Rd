\name{closest}
\alias{closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
closest()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    cols <- c(rep(1, 200), 2)
    cols[unique(apply(dist, 2, which.min))] <- 3
    glyph_colour(g[1]) <- cols
    library(gWidgets)
    dist <- do.call(cbind, lapply(1:length(start.pos), function(i) abs(pom[, 
        i] - start.pos[i])))
    colnames(dist) <- ranges[, 1]
    closest <- apply(dist, 2, which.min)
    win = gwindow(title = "Highlight closest?")
    gg <- ggroup(horizontal = FALSE, container = win)
    for (i in 1:length(closest)) {
        obj = gcheckbox(names(closest)[i], container = gg, action = closest[i], 
            handler = function(h, ...) {
                size <- glyph_size(g[1])
                print(h$obj)
                size[h$action] <- ifelse(svalue(h$obj), 5, 2)
                glyph_size(g[1]) <- size
            })
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
