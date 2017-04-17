library(igraph)

# part(a)
g <- sample_forestfire(10000, fw.prob=0.37, bw.factor=0.32/0.37)
# in-degree distribution
dd1 <- degree_distribution(g, mode="in")
plot(dd1)
# out-degree distribution
dd2 <- degree_distribution(g, mode="out")
plot(dd2)

# part(b)
# find the diameter of the network
diameter <- diameter(g)

# part(c)
# find the community structure of the network
community_structure <- fastgreedy.community(as.undirected(g))

# find the modularity of the structure
modularity <- community_structure$modularity
