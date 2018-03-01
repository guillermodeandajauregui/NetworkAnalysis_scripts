
#take a graph, and a community structure
#return a projection of the graph to the community space 
#edges get weight equal to the number of edges from one network to the other
library(igraph)

#g is a graph, with vertex value infomap

matty = sapply(X = 1:max(V(g)$infomap), function(a){
  sapply(X = 1:max(V(g)$infomap), function(b){
    ifelse(test = a == b, 
           yes = 0, 
           no = length(E(g)[V(g)[infomap==a]%--%V(g)[infomap == b]])
             )
  }
  )
}
)


g_matty = igraph::graph_from_adjacency_matrix(matty, mode = "undirected", weighted = TRUE)
