library(igraph)
library(MASS)

# Reading Facebook graph
setwd("./Documents/EE 232E/EE232-Network-Flow/project1/")
facebook_graph <- read.graph("facebook_combined.txt", directed=FALSE)

# Finding core nodes
core_nodes <- numeric()
for (each_vertex in V(facebook_graph)) {
  if (length(neighbors(facebook_graph, each_vertex)) > 200)
    core_nodes <- c(core_nodes, each_vertex)
}

# Finding average degree of core nodes
degrees_of_core_nodes <- numeric()
degree_of_facebook_graph <- degree(facebook_graph)
for (each_core_nodes in core_nodes) {
  degrees_of_core_nodes <- c(degrees_of_core_nodes, degree_of_facebook_graph[each_core_nodes])
}
average_degree <- mean(degrees_of_core_nodes)

# Randomly pick a core node
randomly_picked_core_node <- sample(core_nodes, 1)

# Construct a personal network using the picked core node
neighborhood_of_core_node <- neighborhood(facebook_graph, order = 1, nodes = randomly_picked_core_node)
personal_network <- induced.subgraph(facebook_graph, unlist(neighborhood_of_core_node))
personal_network$label = sort(unlist(neighborhood_of_core_node))

# Fast greedy
fast_greedy_community <- fastgreedy.community(personal_network)
modularity(fast_greedy_community)
sizes(fast_greedy_community)
plot(fast_greedy_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Edge betweenness
edge_betweenness_community <- edge.betweenness.community(personal_network)
modularity(edge_betweenness_community)
sizes(edge_betweenness_community)
plot(fast_greedy_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Infomap
infomap_community <- infomap.community(personal_network)
modularity(infomap_community)
sizes(infomap_community)
plot(infomap_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)
