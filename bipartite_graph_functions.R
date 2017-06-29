#bipartite graph functions
#based on Latapy 2008
library(igraph)

#model graphs
bg = bipartite.random.game(n1 = 10, n2 = 100, type = "gnp", p = 0.1)
V(bg)[type == FALSE]$name <- as.character(as.roman(1:10))

a = combn(LETTERS[1:26], m = 2, simplify = TRUE)
c= character()
for(i in 1:325){b = paste0(a[1,i], a[2,i])
  c = c(c,b)
  rm(i)
}
V(bg)[type == TRUE]$name = c[1:100]

plot(bg)

#Neighbors of distance 2
NeighborsDistance2 = function(g, v){
  a = neighbors(graph = g, v = v)
  b = adjacent_vertices(graph = g, v = a)
  c = unique(unlist(b))
  #return(c)
  if(!is.null(c)){
    c = c[order(c)]  
  }
   d2 = V(g)[c]
   return(d2)
}


