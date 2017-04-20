library(igraph)

# part(a)
forest_fire_graph <- forest.fire.game(1000, fw.prob=0.37, bw.factor=0.32/0.37)
# in-degree distribution
in_degree <- degree.distribution(forest_fire_graph, mode="in")
barplot(in_degree, main="In Degree Distribution")

# out-degree distribution 
out_degree <- degree.distribution(forest_fire_graph, mode="out")
barplot(out_degree, main="Out Degree Distribution")

# part(b)
# find the diameter of the network
diameter <- diameter(forest_fire_graph)

# part(c)
# find the community structure of the network
community_structure <- fastgreedy.community(as.undirected(forest_fire_graph))

# find the modularity of the structure
modularity <- community_structure$modularity