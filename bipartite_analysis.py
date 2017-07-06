# -*- coding: utf-8 -*-
"""
Script to take a nx-style gml of a bipartite graph, calculate 
"""

import networkx as nx
#import argparse
#parser =  argparse.ArgumentParser()
#parser.add_argument("graph")
#parser.add_argument("out")
#args = parser.parse_args()
#path = args[1]
#path_out = args[2]

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


nx.set_node_attributes(F, 
                       "Redundancy", 
                       values = nx.bipartite.node_redundancy(F)
                       )


nx.write_gml(F, path_out)
