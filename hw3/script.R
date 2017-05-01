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

# Problem 5
# Creating a list to store all subgraphs
sub_graphs <- c()
# Creating a list to store all sub-communities
sub_communities <- c()
# Getting the indexes of community that has 100+ size
indexes_community_fastgreedy2_larger_than_100 <- which(sizes(community_fastgreedy2) > 100)

# For every community that has size 100 or greater
for (index in indexes_community_fastgreedy2_larger_than_100) {
  vertices_of_sub_graphs <- c()
  for (each_vertex in V(ug2)) {
    if (community_fastgreedy2$membership[each_vertex] == index) {
      vertices_of_sub_graphs <- append(vertices_of_sub_graphs, each_vertex)
    }
  }
  # A subgraph obtained from looking at all vertices
  sub_graph <- induced.subgraph(ug2, vids=vertices_of_sub_graphs)

  # Finding sub-community from subgraph
  sub_community <- fastgreedy.community(sub_graph)
  # Print size of the found sub-community along with number of nodes belonging in subcommunity
  print(size(sub_community))
}

# Problem 6
nodes_belonging_to_multiple_community <- c()
threshold <- 0.1
for (each_vertex in 1:length(V(graph))) {
  teleportation_probability = rep(0, vcount(graph))
  teleportation_probability[each_vertex] = 1
  random_walk <- netrw(graph, walker.num = 1, start.node=each_vertex, damping = 0.85, teleport.prob = teleportation_probability, output.visit.prob = TRUE)
  sorted_visit_probability <- sort(random_walk$ave.visit.prob, decreasing = TRUE, index.return = TRUE)
  M_i <- rep(0, length(community_fastgreedy2))
  
  for (j in 1:30) {
   m_j <- rep(0, length(community_fastgreedy2))
   vertex_index <- which(V(gcc) == V(graph)[sorted_visit_probability$ix[j]])
   m_j[community_fastgreedy2$membership[vertex_index]] <- 1
   M_i <- M_i + sorted_visit_probability$x[j] * m_j
  }
  
  if (length(which(M_i > threshold)) >= 2) {
    nodes_belonging_to_multiple_community <- rbind(nodes_belonging_to_multiple_community, c(each_vertex, M_i))
  }
}

nodes_belonging_to_multiple_community