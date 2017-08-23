dijkstra <- function(graph, init){
# ========================================================================================
## Krzysztof Bartoszek 2017.08.03
# original code
# produces error: Error in is.data.frame(x) && do.NULL : invalid 'y' type in 'x && y'
# also column names of graph are v1,v2,w
#  stopifnot(colnames(graph, c("v1, v3, w")))
# =============================================
# corrected code that works
stopifnot(setequal(colnames(wiki_graph), c("v1", "v2", "w")))
# ========================================================================================
  no_vertices <- max(graph[,1], graph[,2])
  
  dist <- numeric(no_vertices) + Inf
  dist[init] <- 0
  
  Q <- 1:no_vertices
  
  while(length(Q) > 0){
    u <- which(min(dist[Q]) == dist)[1]
    Q <- Q[-which(Q == u)]
    for(v in graph[graph[,1] == u, 2]){
      alt <- dist[u] + graph[graph[,1]==u & graph[,2]==v, 3]
      if(alt < dist[v]){
        dist[v] <- alt
      }
    }
  }
  dist
}

