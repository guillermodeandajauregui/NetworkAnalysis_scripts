#########################################
#Community similarity and evolution
#from Takaffoli 2011
#########################################

#Five changes in communities
#i --> j
#Formation
# no match in i for community in j
#Dissolution
# no match in j for community in i
#Survival
# community in i with match in j
#Splitting
# set of communities in j where at least k proportion of members come from comm in i, 
#and intersection ( union comms in j, comm in i) equal and greater to k
#Merging
#opposite of splitting, from j to i 

#They are NOT mutually exclusive (except formation/survival and dissolution)

cluster_sim <- function(set_a, set_b, k){
  
  sect = length(base::intersect(set_a, set_b))
  mx = max(length(set_a), length(set_b))
  ifelse(test = sect >= k, yes = sect/mx, 0)
}



split_or_merge_function <-function(comms_i, comms_j, k){
#comms_i = comms from graph i
#comms_j = comms from graph j
  f1 = function(i,j){
    return(length(base::intersect(i,j))/length(j))
  }
  
  mat_1 = matrix(nrow = length(comms_i), 
                 ncol = length(comms_j), 
                 dimnames = list(names(comms_i), 
                                 names(comms_j)))
  
                 for(i in seq_along(comms_i)){
                   for(j in seq_along(comms_j)){
                     mat_1[i,j]<-f1(comms_i[[i]], comms_j[[j]])
  
                   }
                 }
                 
    f2 = function(i,j){
      return(length(base::intersect(i,j))/length(i))
    }
    mat_2 = matrix(nrow = length(comms_i), 
                   ncol = length(comms_j), 
                   dimnames = list(names(comms_i), 
                                   names(comms_j)))
                                
                   for(i in seq_along(comms_i)){
                    for(j in seq_along(comms_j)){
                      mat_2[i,j]<-f2(comms_i[[i]], comms_j[[j]])
                                  }
                                }
                 
 
  #test_1 = names(which(mat_1>k, arr.ind = TRUE)[,2])
  test_1 = apply(X = mat_1, MARGIN = 1, FUN = function(x) names(which(x>k, arr.ind = TRUE))) #list, for every comm_i, which comms_j pass test
  test_2 = names(rowSums(mat_2)[which(rowSums(mat_2)>k)]) #named vector, which comm_i pass test 2 
  resultado = test_1[test_2]
  resultado = resultado[lapply(resultado, length)>1]
  return(resultado)
  
  
}


community_list <-function(g, 
                          grouping = "infomap", 
                          comm_naming = deparse(
                                        substitute(g)
                                        )
                          )
  {
  
  if(grouping != "infomap"){
    rr = deparse(substitute(g))
    q = paste0("V(", rr, ")$", grouping)            #this looks awful, but it works.
    qq = eval(parse(text = q))
    
    
    lcomm =lapply(X = 1:max(qq),
                  FUN = function(x){
                    V(g)$name[which(qq==x)]
                  }
    )  
  }
  
  
  lcomm =lapply(X = 1:max(V(g)$infomap),
         FUN = function(x){
           V(g)$name[which(V(g)$infomap==x)]
         }
  )

  names(lcomm)<- paste0(comm_naming, "_", 1:length(lcomm))

  return(lcomm)
  
}
  
network_community_evolution_analysis <-function(gi, gj, k, grouping = "infomap"){
  #takes two network objects, with a grouping (community) vertex attribute, and a k value
    #use the following convention: 
      #gi is first graph (ie, before), 
      #gj is last time (ie, after)
  #list communities gi as list of character vectors, named gi_1:N
  comms_gi = community_list(g = gi, grouping = grouping)
  #list communities gj as list of character vectors, named gj_1:N
  comms_gj = community_list(g = gj, grouping = grouping)
  #make community similarity matrix (csm)
  csm = matrix(nrow = length(comms_gi), 
                  ncol = length(comms_gj), 
                  dimnames = list(names(comms_gi), 
                                  names(comms_gj)
                  )
  )
  
  for(i in seq_along(comms_gi)){
    for(j in seq_along(comms_gj)){
      csm[i,j]<-cluster_sim(comms_gi[[i]], comms_gj[[j]], k = k)
    }
  }
  
  #filter csm by k 
  simi_k = ifelse(csm < k, 0, csm)
  
  # formation: named list of communities in gj
  formation = names(which(colSums(simi_k)==0))
  # dissolution: named list of communities in gi 
  dissolution = names(which(rowSums(simi_k)==0))
  #survival: dataframe of communities in gi, and corresponding community in gj
  survival = which(simi_k>0, arr.ind = TRUE)
  survival[,1]<-apply(survival, 1, function(qq) {
    rownames(simi_k)[as.numeric(qq[1])]
  })
  survival[,2]<-apply(survival, 1, function(qq) {
    colnames(simi_k)[as.numeric(qq[2])]
  })
  rownames(survival)<-1:length(rownames(survival))
  colnames(survival)<-c(deparse(substitute(gi)), deparse(substitute(gj)))
  survival = as.data.frame(survival)
  #split: named list of communities in gi, in each the name of the communities derived in gj
  split = split_or_merge_function(comms_i = comms_gi, comms_j = comms_gj, k = k)
  #merge: named list of communities in gj, in each the name of the original communities in gi
  merge = split_or_merge_function(comms_i = comms_gj, comms_j = comms_gi, k = k)
  
  #returns a list of communities #a) formed in gj, b)dissolved in gj, c) survived in gj), d)split in gj, e) merged in gj
  #comms in gi
  #comms in gj
  #nodes lost from gi to gj
  nodes_lost = setdiff(unlist(comms_gi), unlist(comms_gj))
  #nodes emerged in gi to gj
  nodes_gained = setdiff(unlist(comms_gj), unlist(comms_gi))
  return(list(formation = formation, 
              dissolution = dissolution, 
              survival = survival, 
              split = split, 
              merge = merge,
              comms_gi = comms_gi,
              comms_gj = comms_gj,
              nodes_lost = nodes_lost,
              nodes_gained = nodes_gained))
}


formation_function24 <- function(simi_k, k){
  
  #return which communities in j where formed
  #communities in j with no match in i (all zeros)
  #consider columns as communities in time j 
  return(names(which(colSums(simi_k)==0)))
}

#partial functions
#1) make community simmilarity matrix, k = 0 

comm_sim_matrix <- function(graph_a, graph_b, community_value = "infomap", k = 0){
  
  
  comms_a = lapply(X = 1:max(V(graph_a)$infomap), 
                   FUN = function(x){
                     V(graph_a)$name[which(V(graph_a)$infomap==x)]
                   }
  )
  
  comms_b = lapply(X = 1:max(V(graph_b)$infomap), 
                   FUN = function(x){
                     V(graph_b)$name[which(V(graph_b)$infomap==x)]
                   }
  )
  
  names(comms_a) <- paste0(1:length(comms_a), "_", deparse(substitute(graph_a)))
  names(comms_b) <- paste0(1:length(comms_b), "_", deparse(substitute(graph_b)))
  
  matriz = matrix(nrow = length(comms_a), 
                  ncol = length(comms_b), 
                  dimnames = list(names(comms_a), 
                                  names(comms_b)
                  )
  )
  
  for(i in seq_along(comms_a)){
    for(j in seq_along(comms_b)){
      matriz[i,j]<-cluster_sim(comms_a[[i]], comms_b[[j]], k = k)
    }
  }
  return(matriz)
}

#2) 

formation_function <- function(community_similarity_matrix, k){
  simi_k = ifelse(community_similarity_matrix < k, 0, community_similarity_matrix)
  #return which communities in j where formed
  #communities in j with no match in i (all zeros)
  #consider columns as communities in time j 
  return(names(which(colSums(simi_k)==0)))
}

dissolution_function <- function(community_similarity_matrix, k){
  simi_k = ifelse(community_similarity_matrix < k, 0, community_similarity_matrix)
  #return which communities in i were dissolved
  #communities in i with no match in j (all zeros)
  #consider rows as communities in time i 
  return(names(which(rowSums(simi_k)==0)))
}

survival_function <-function(community_similarity_matrix, k){
  simi_k = ifelse(community_similarity_matrix < k, 0, community_similarity_matrix)
  #return which communities in i which survived into j
  #only keep those that have 1 community.. otherwise it's split
  q = which(simi_k>0, arr.ind = TRUE)
  reps = as.numeric=names(which(table(q[,1])>1))  #remove communities in i with more than one comm in j
  q = q[-which(q[,1]==reps),]
  q[,1]<-apply(q, 1, function(qq) {
    rownames(simi_k)[as.numeric(qq[1])]
  })
  q[,2]<-apply(q, 1, function(qq) {
    rownames(simi_k)[as.numeric(qq[2])]
  })
  rownames(q)<-1:length(rownames(q))
  colnames(q)<-c("gi", "gj")
  #apply(q, MARGIN = 1, FUN = function(q) print(q[1,1], q[1,2]))
  #return(rownames(q)[which(table(which(mt>0, arr.ind = TRUE)[,1])==1)])
  return(q)
}


######cleanup#########



#deprecated

# split_or_merge_function<-function(comms_a, comms_b, k){
#   #first argument is the reference network. Earlier for split, later for merge. 
#   
#   #get values of Vi^Vj / Vj in a list of lists
#   
#   q = lapply(X = comms_a, 
#              FUN = function(a){
#                rat = lapply(X = comms_b, 
#                             FUN = function(b){
#                               ret = length(base::intersect(a, b))/length(b)
#                               return(ret)
#                             }
#                )
#                return(rat)
#              }
#   )
#   
#   names(q) <- names(comms_a)
#   #which are above k ?
#   #do they comply with U(Cj*)^vi/vi > k ?
#   
#     test_2 = mapply(FUN = function(caa, idx_bb){ #returns a vector, length comms_a, with value U(Cj*)^vi/vi
#                     aa = caa
#                     union_b = unique(unlist(comms_b[idx_bb]))
#                     cc = base::intersect(x = aa, union_b )
#                     divis = length(cc)/length(aa)
#                     }, 
#                     comms_a, 
#                     comm_b_idx_list
#                     )
#     
#     return(names(comms_a)[which(test_2>k)])
#     #return a list of each comm_a, and the names of the communities in b into which it split/merged. 
#     #right now it is not invertible. look into it.
# }
