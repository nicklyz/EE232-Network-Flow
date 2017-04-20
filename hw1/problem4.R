library(igraph)

# part(a)
forest_fire_graph <- forest.fire.game(1000, fw.prob=0.37, bw.factor=0.32/0.37)
plot(forest_fire_graph, vertex.label=NA, main="Forest Fire Network")

# in-degree distribution
in_degree <- degree.distribution(forest_fire_graph, mode="in")
barplot(in_degree, main="In Degree Distribution of Forest Fire Graph")

# out-degree distribution
out_degree <- degree.distribution(forest_fire_graph, mode="out")
barplot(out_degree, main="Out Degree Distribution of Forest Fire Graph")

# part(b)
# find the diameter of the network
diameter <- diameter(forest_fire_graph)

# part(c)
# find the community structure of the network
community_structure <- spinglass.community(forest_fire_graph)
plot(community_structure, forest_fire_graph, vertex.label=NA, main="Community Structure for Forest Fire Network")

# find the modularity of the structure
modularity <- modularity(community_structure)
