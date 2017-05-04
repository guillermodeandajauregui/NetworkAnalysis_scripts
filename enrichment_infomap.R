#function_enrichment_infomap
#takes an analyzed network
#and a Named list of NodeSets (for instance, biological pathways)
#returns a list of enriched pathways through hypergeometric testing

function_enrichment_infomap = function(nw_an, NodeSets){
  list_enriched_pws = list()
  for(i in 1:max(nw_an$infomap$membership)){
    genes_isla = names(V(nw_an$g))[which(V(nw_an$g)$infomap==i)]
    h8 = multiHyperGeoTest(collectionOfGeneSets = NodeSets,
                           universe = names(V(nw_an$g)), #nodes IN network
                           hits = genes_isla, # nodes in island 
                           minGeneSetSize = 10, #all pathways larger than 10
                           pAdjustMethod = "BH", 
                           verbose = FALSE)
    h8 = as.data.frame(h8)
    h8 = h8[h8$Adjusted.Pvalue < 0.1,]
    list_enriched_pws = c(list_enriched_pws, list(h8))
  }
  names(list_enriched_pws)<-1:max(nw_an$infomap$membership)
  return(list_enriched_pws)
}
