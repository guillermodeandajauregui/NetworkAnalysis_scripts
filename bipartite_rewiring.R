#rewiring function for bipartite graphs
#to generate null models preserving degree distibution for both node sets

empty_rewire_graph = function(g){
  e = delete.edges(g, E(g))
  V(e)$socket = degree(g)
  return(e)
}

funcion_rewire = function(g){
  
  #change type of nodes to "TOP" or "BOT"
  types = names(table(V(g)$type))
  V(g)$type = ifelse(V(g)$type == types[1], "TOP", "BOT")
  
  for(punto in V(g)[type == "TOP"][order(V(g)[type == "TOP"]$socket, decreasing = TRUE)]){
    top_socket = V(g)[punto]$socket
    #find x with open sockets
    a = which(V(g)[type == "BOT"]$socket > 0)
    b = V(g)[type == "BOT"][a]
    b = as.numeric(b)
    print(b)
    #sample top_socket
    print(top_socket)
    #take care of sampling when b is only one!
    if(length(b) == 1){
      c = b
    }else
      c = sample(b, top_socket)
    c = c[order(c)]
    c = as.numeric(c)
    print(c)
    #connect 
    for(tache in c){
      print(c(punto, tache))
      g = add.edges(g, c(punto, tache))
      #update sockets, tache
      V(g)[tache]$socket = V(g)[tache]$socket -1
      #update sockets, punto
      V(g)[punto]$socket = V(g)[punto]$socket - 1
    }
  }
  return(g)
}

bipartite_rewire_model = function(b){
  g = empty_rewire_graph(b)
  g = funcion_rewire(g)
  return(g)
}


