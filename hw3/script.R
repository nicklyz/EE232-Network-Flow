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
cluster <- clusters(graph);
gcc <- induced_subgraph(graph, which(cluster$membership == which.max(cluster$csize)))
# compare graph size with gcc
vcount(graph)
vcount(gcc)

# Problem 2
# in degree distribution
in_dist = degree.distribution(gcc, mode="in")
plot(in_dist, main="In Degree Distribution of GCC", ylab="distribution", xlab="In Degree")
# out degree distribution
out_dist = degree.distribution(gcc, mode="out")
plot(out_dist, main="Out Degree Distribution of GCC", ylab="distribution", xlab="Out Degree")


# NOTE: get weights of the Edges E(g)$Weight

# Problem 3
# Option 1
ug1 <- as.undirected(gcc, mode = "each")
community_propagation1 <- label.propagation.community(ug1, weights=E(ug1)$weights)

# Option 2
comb_weight <- function(weights) {
  return (sqrt(prod(weights)))
}

ug2 <- as.undirected(gcc, mode = "collapse", edge.attr.comb = comb_weight)
#fastgreedy.community
community_fastgreedy2 <- fastgreedy.community(ug2, weights = E(ug2)$weights)
#label.propagation.community
community_propagation2 <- label.propagation.community(ug2, weights=E(ug2)$weights)

# Problem 4
largest_community <- induced.subgraph(ug2, which(community_fastgreedy2$membership == which.max(sizes(community_fastgreedy2))))
sub_community <- fastgreedy.community(largest_community, weights=E(largest_community)$weights)
sizes(sub_community)



