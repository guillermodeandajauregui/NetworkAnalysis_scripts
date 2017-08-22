# -*- coding: utf-8 -*-
"""
Script to take a nx-style gml of a bipartite graph, calculate 
"""

import networkx as nx
import argparse
parser =  argparse.ArgumentParser()
parser.add_argument("graph")
parser.add_argument("out")
args = parser.parse_args()
path = args.graph
path_out = args.out

B = nx.read_gml(path)

nx.set_node_attributes(B, 
                       "degree", 
                       values = nx.degree(B)
                       )


F = B.subgraph((n for n in B if B.node[n]['degree']>1))
F = F.to_undirected()
#nx.nodes(D)
nx.set_node_attributes(F, 
                       "ClusteringDot", 
                       values = nx.bipartite.clustering(F, mode = "dot")
                       )

#if redundancy fails with  Cannot compute redundancy coefficient for a node that has fewer than two neighbors
#redundancia= dict()
#for nodo in F.nodes():
#    if F.node[nodo]["degree"]>2:
#        redundancia[nodo] = nx.bipartite.node_redundancy(F, nodes = nodo)
#    else:
#        redundancia[nodo] = 0

#nx.set_node_attributes(F, 
#                       "Redundancy", 
#                       values = redundancia
#                       )

try:
    nx.set_node_attributes(F, 
                       "Redundancy", 
                       values = nx.bipartite.node_redundancy(F)
                       )
except Exception:
    nx.set_node_attributes(F, "Redundancy", 0) 
    for nodo in F.nodes():
        if nx.degree(F, nodo) > 2:
            F.node[nodo]["Redundancy"]  = nx.bipartite.node_redundancy(F, [nodo])[nodo]
        else:
            F.node[nodo]["Redundancy"]  = 7 # or none, or NA  ... just to keep it as numeric, and remove later

nx.write_gml(F, path_out)


