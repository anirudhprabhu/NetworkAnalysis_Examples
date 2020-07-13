# Network Community Detection Examples

library(igraph)
#Set seed for reproducibility
set.seed(123)
# Creating a Random Network for the example
g <- erdos.renyi.game(50, 40, type = "gnm")
plot(g)

# Run Community Detection using Louvain Algorithm
# This function implements the multi-level modularity optimization algorithm for finding community structure, see VD Blondel, J-L Guillaume, R Lambiotte and E Lefebvre: Fast unfolding of community hierarchies in large networks, http://arxiv.org/abs/arXiv:0803.0476 for the details.
Louvain_g<-cluster_louvain(g)
plot(Louvain_g,g)

# Community assignment for each node
Louvain_g$membership

# Examine modularities
G_ModMat_Louvain <- mod.matrix(graph = g,membership = Louvain_g$membership)
Louvain_g$modularity

# Examining the group/communities
groups(Louvain_g) 

# Run Walktrap Community Detection Algorithm
Walktrap_g <- cluster_walktrap(g)
plot(Walktrap_g,g)

# Community assignment for each node
Walktrap_g$membership

#Examining Modularities
G_ModMat_WT <- mod.matrix(graph = g,membership = Walktrap_g$membership)
Walktrap_g$modularity

# Examining the groups
groups(Louvain_g) 

# Additional community detection algorithms can be run in the same way. 


