library(igraph)
library(MASS)

facebook_graph <- read.graph("facebook_combined.txt", directed=FALSE)
core_nodes <- numeric()
for (each_vertex in V(facebook_graph)) {
  if (length(neighbors(facebook_graph, each_vertex)) >= 200)
    core_nodes <- c(core_nodes, each_vertex)
}

degrees_of_core_nodes <- numeric()
