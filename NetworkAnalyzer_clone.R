#NetworkAnalyzer clone
#Network parameters calculated by the Cytoscape Network Analyzer
#takes Igraph object
#returns list, including igraph (with centrality measures for vertices), 
#average path length, clustering coefficient, Graph diameter
#component distribution
#and Infomap communities

library(igraph)

NetworkAnalyzer = function(g, directed = FALSE){
  if(directed == FALSE){
    V(g)$degree <- degree(g)
    V(g)$betweenness <- betweenness(g, directed = FALSE)
    V(g)$closeness <- closeness(g)
    
    average_path_length = average.path.length(g, directed = FALSE)
    ClusteringCoefficient = transitivity(g, type = "global")
    GraphDiameter = diameter(g, directed = FALSE)
    
    component = components(g)
    V(g)$component = component$membership
    
    infomap = infomap.community(g)
    V(g)$infomap = infomap$membership
    
    results = list(g = g, 
                   average_path_length = average_path_length, 
                   ClusteringCoefficient = ClusteringCoefficient, 
                   GraphDiameter = GraphDiameter,
                   component = component,
                   infomap = infomap)
  }
}



