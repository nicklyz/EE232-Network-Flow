library(igraph)
library(netrw)

# Problem 1
# load data
df = read.table("sorted_directed_net.txt", sep='\t', header=FALSE)
colnames(df) = c("Node1", "Node2", "Weight")
# create graph from data
graph = graph.data.frame(df, directed=TRUE)
# connectivity of graph
connectivity <- is.connected(graph)

# get giant connected component
if (!connectivity) {
  cl <- clusters(graph);
  gccIndex <- which.max(cl$size)
  nonGccNodes <- (1:vcount(graph))[cl$membership != gccIndex]
  gcc <- delete.vertices(graph, nonGccNodes)
}

# Problem 2
# in degree distribution
in_dist = degree.distribution(gcc, mode="in")
plot(in_dist, main="In Degree Distribution")
# out degree distribution
out_dist = degree.distribution(gcc, mode="out")
plot(out_dist, main="Out Degree Distribution")


# NOTE: get weights of the Edges E(g)$Weight