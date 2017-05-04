#plot nicely 
#takes the results of the Network Analyzer clone and returns a quick plot 
#no vertex labels, vertex color based on infomap communities

plot_nicely<-function(Annie){#result of Network Analyzer
  paleta = rainbow(n = max(Annie$infomap$membership))
  V(Annie$g)$color <- paleta[Annie$infomap$membership]
  plot(Annie$g, vertex.label = "", vertex.size = 4, edge.width = 0.5)
}
