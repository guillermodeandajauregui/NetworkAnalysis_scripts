#bipartite graph functions
#based on Latapy 2008
library(igraph)

#model graphs for testing
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

biguglytest = bipartite.random.game(n1 = 100, n2 = 1000, type = "gnp", p = 0.1)
V(biguglytest)$name = paste0("ugly", 1:1100)

prettytest = bipartite.random.game(n1 = 5, n2 = 10, type = "gnp", p = 0.1)
V(prettytest)$name = paste0("pretty", 1:15)
plot(prettytest)

gargantua = bipartite.random.game(n1 = 100, n2 = 22000, type = "gnp", p = 0.1)
V(gargantua)$name = paste0("ugly", 1:22000)

##############Neighbors of distance 2

NeighborsDistance2 <- function(g, v){
  #takes a single node; 
  if(length(v)>1){
    warning("More than one V selected; returning distance 2 neighborhood for first vertex only")
  }
  vx = V(g)[v] #define v as a igraph.vs
  a = neighbors(graph = g, v = vx) #gets neighbors for v
  b = adjacent_vertices(graph = g, v = a) # gets neighbors of neighbors of v, including v
  c = unique(unlist(b)) #
  #return(c)
   if(!is.null(c)){
     c = c[order(c)]  
   }
  
    d2 = V(g)[c] #still includes v
    d2 = d2[!(d2%in%vx)] #removes v
    return(d2)
}

NeighborsDistance2.multi <- function(g, v=V(g), size = FALSE){

    a = lapply(V(g)[v], NeighborsDistance2, g = g)
  if(size == FALSE){
    return(a)
  }
  if(size == TRUE){
    return(sapply(a, length))
  }
}

#takes about 15 minutes to analyze a 22100 node network.
proctor = proc.time()
test_temp = NeighborsDistance2.multi(gargantua, size = TRUE)
proctor = proc.time() - proctor
#
plot(x = degree(prettytest, v = V(prettytest)[type == TRUE]), 
     y = NeighborsDistance2.multi(prettytest, size = TRUE, v = V(prettytest)[type == TRUE]) 
     )

plot(x = degree(bg, v = V(bg)[type == TRUE]), 
     y = NeighborsDistance2.multi(bg, size = TRUE, v = V(bg)[type == TRUE]) 
)

plot(x = degree(biguglytest, v = V(biguglytest)[type == TRUE]), 
     y = NeighborsDistance2.multi(biguglytest, size = TRUE, v = V(biguglytest)[type == TRUE]) 
)

##############Clustering coefficient, point

ClusteringCoefficientPoint.Pair <- function(g, v1, v2){
  sect = V(g)[nei(v1) & nei(v2)]
  nion = V(g)[nei(c(v1,v2))] 
  point = length(sect)/length(nion)
  return(point)
}

ClusteringCoefficientPoint.Pair(bg, 2, 4)
sapply(X = V(bg)[1:5], FUN = ClusteringCoefficientPoint.Pair, g = bg, v1 = 2, USE.NAMES = TRUE)

sapply(X = V(bg)[NeighborsDistance2(g = bg, v = "II")], 
       FUN = ClusteringCoefficientPoint.Pair, 
       g = bg, 
#       v1 = V(bg)["II"], 
       v1 = 2,
       USE.NAMES = TRUE)

ClusteringCoefficientPoint <- function(g, u){
  u = V(g)[u]
  NNu = (NeighborsDistance2(g = g, v = u))
  if(length(NNu)==0){
    return(0)
  }
  ccp = sapply(X = V(g)[NNu], 
               FUN = function(v){ClusteringCoefficientPoint.Pair(g = g, 
                                                                 v1 = V(g)[v], 
                                                                 v2 = V(g)[u])},
               USE.NAMES = TRUE
                 )
  #if(length(ccp)==0){ccp = 0}
  #if(length(NNu)==0){NNu = 1}
  ccpu = (sum(ccp))/(length(NNu))
  return(ccpu)
}

ClusteringCoefficientPoint(bg, 11)
length(ClusteringCoefficientPoint(bg, 11))
V(bg)[type == "TRUE"]
mean(sapply(X = V(bg)[type == "FALSE"], ClusteringCoefficientPoint, g = bg))
a = sapply(X = V(bg)[type == TRUE], ClusteringCoefficientPoint, g = bg)

#Helper cumulative distribution function

cumulative.dist <- function(x, by.breaks = 0.01){
  breaks = seq(0,1, by = by.breaks)
  x.cut = cut(x, breaks, right = FALSE)
  x.freq = table(x.cut)
  x.cumsum = cumsum(x.freq)
  return(x.cumsum)
}

#tests proctor = proc.time()
proctor = proc.time()
test_temp = sapply(X = V(gargantua), FUN = ClusteringCoefficientPoint, g = gargantua)
proctor = proc.time() - proctor




cumulative.dist(a, by.breaks = 0.001)
