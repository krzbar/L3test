\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TYT1
%%  ~~function to do ... ~~
}
\description{DESC1
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init}{
%%     ~~Describe \code{init} here~~
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
function (graph, init) 
{
    stopifnot(setequal(colnames(wiki_graph), c("v1", "v2", "w")))
    no_vertices <- max(graph[, 1], graph[, 2])
    dist <- numeric(no_vertices) + Inf
    dist[init] <- 0
    Q <- 1:no_vertices
    while (length(Q) > 0) {
        u <- which(min(dist[Q]) == dist)[1]
        Q <- Q[-which(Q == u)]
        for (v in graph[graph[, 1] == u, 2]) {
            alt <- dist[u] + graph[graph[, 1] == u & graph[, 
                2] == v, 3]
            if (alt < dist[v]) {
                dist[v] <- alt
            }
        }
    }
    dist
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
