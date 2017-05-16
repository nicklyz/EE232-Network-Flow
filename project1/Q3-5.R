library(igraph)

# Reading Facebook graph
setwd("./Documents/EE 232E/EE232-Network-Flow/project1/")
facebook_graph <- read.graph("facebook_combined.txt", directed=FALSE)
facebook_graph$label <- V(facebook_graph)

# Question 3

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
randomly_picked_core_node <- 349
# randomly_picked_core_node <- sample(core_nodes, 1)

# Construct a personal network using the picked core node
neighborhood_of_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = randomly_picked_core_node))
personal_network <- induced.subgraph(facebook_graph, neighborhood_of_core_node)
personal_network$label <- sort(neighborhood_of_core_node)

# Fast greedy
fast_greedy_community <- fastgreedy.community(personal_network)
modularity(fast_greedy_community)
sizes(fast_greedy_community)
plot(fast_greedy_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Edge betweenness
edge_betweenness_community <- cluster_edge_betweenness(personal_network)
modularity(edge_betweenness_community)
sizes(edge_betweenness_community)
plot(edge_betweenness_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Infomap
infomap_community <- infomap.community(personal_network)
modularity(infomap_community)
sizes(infomap_community)
plot(infomap_community, personal_network, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Question 4
# Removing the core node from the graph
neighborhood_without_core_node <- neighborhood_of_core_node[neighborhood_of_core_node != randomly_picked_core_node]

# Construct a personal network using the picked core node
personal_network_without_core_node <- induced.subgraph(facebook_graph, neighborhood_without_core_node)
personal_network_without_core_node$label <- sort(neighborhood_without_core_node)

# Fast greedy
fast_greedy_community_without_core_node <- fastgreedy.community(personal_network_without_core_node)
modularity(fast_greedy_community_without_core_node)
sizes(fast_greedy_community_without_core_node)
plot(fast_greedy_community_without_core_node, personal_network_without_core_node, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Edge betweenness
edge_betweenness_community_without_core_node <- cluster_edge_betweenness(personal_network_without_core_node)
modularity(edge_betweenness_community_without_core_node)
sizes(edge_betweenness_community_without_core_node)
plot(edge_betweenness_community_without_core_node, personal_network_without_core_node, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Infomap
infomap_community_without_core_node <- infomap.community(personal_network_without_core_node)
modularity(infomap_community_without_core_node)
sizes(infomap_community_without_core_node)
plot(infomap_community_without_core_node, personal_network_without_core_node, edge.color = "light grey", vertex.label=NA, vertex.size=2, asp=9/16)

# Question 5
# three_picked_core_nodes <- sample(core_nodes, 3)

all_embeddednesses <- numeric()
all_dispersions <- numeric()

# For each core nodes
for (each_core_node in core_nodes) {
  # Create a personal network using the core node
  neighborhood_of_each_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = each_core_node))
  personal_network_with_each_core_node <- induced.subgraph(facebook_graph, neighborhood_of_each_core_node)
  personal_network_with_each_core_node$label <- sort(neighborhood_of_each_core_node)
  each_core_node_neighbors <- neighbors(facebook_graph, each_core_node)
  
  for (each_node in personal_network_with_each_core_node$label) {
    if (each_core_node != each_node) {
      # Calculating embeddedness of each personal network
      each_node_neighbors <- neighbors(facebook_graph, each_node)
      intersection_of_neighbors <- intersect(each_core_node_neighbors, each_node_neighbors)
      all_embeddednesses <- c(all_embeddednesses, length(intersection_of_neighbors))
    }
  }
}

for (each_core_node in core_nodes) {
  # Create a personal network using the core node
  neighborhood_of_each_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = each_core_node))
  personal_network_with_each_core_node <- induced.subgraph(facebook_graph, neighborhood_of_each_core_node)
  personal_network_with_each_core_node$label <- sort(neighborhood_of_each_core_node)
  each_core_node_neighbors <- neighbors(facebook_graph, each_core_node)
  
  for (each_node in personal_network_with_each_core_node$label) {
    if (each_core_node != each_node) {
      each_node_neighbors <- neighbors(facebook_graph, each_node)
      intersection_of_neighbors <- intersect(each_core_node_neighbors, each_node_neighbors)
      
      # Removing the node and the core node from the network
      vertices_to_delete <- c(each_node, each_core_node)
      facebook_graph_without_each_core_node_and_each_node <- delete.vertices(facebook_graph, vertices_to_delete)
      facebook_graph_without_each_core_node_and_each_node$label <- sort(intersect(which(facebook_graph$label != each_core_node), which(facebook_graph$label != each_node)))
      
      disp <- 0
      # If they have 1 mutual friend, distance is 0
      if (length(intersection_of_neighbors) >= 2) {
        combination_of_intersections = combn(intersection_of_neighbors, 2)
        for (index in 1:length(combination_of_intersections)) {
          if (index %% 2) {
            first_node <- combination_of_intersections[index]
            second_node <- combination_of_intersections[index + 1]
            length_of_path <- distances(facebook_graph_without_each_core_node_and_each_node, c(first_node), c(second_node))
            if (length_of_path[1] != Inf) {
              disp <- length_of_path[1] + disp
            }
          }
        }
      }
      
      all_dispersions <- c(all_dispersions, disp)
    }
  }
}

hist(all_embeddednesses, breaks = 50, main = "Distribution of Embeddedness", xlab = "Embeddedness", col="green")
hist(all_dispersions, breaks = 50, main = "Distribution of Dispersion", xlab = "Dispersion", col="green")

for (each_core_node in picked_core_nodes) {
  # Create a personal network using the core node
  neighborhood_of_each_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = each_core_node))
  personal_network_with_each_core_node <- induced.subgraph(facebook_graph, neighborhood_of_each_core_node)
  personal_network_with_each_core_node$label <- sort(neighborhood_of_each_core_node)
  each_core_node_neighbors <- neighbors(facebook_graph, each_core_node)
  
  for (each_node in personal_network_with_each_core_node$label) {
    if (each_core_node != each_node) {
      each_node_neighbors <- neighbors(facebook_graph, each_node)
      intersection_of_neighbors <- intersect(each_core_node_neighbors, each_node_neighbors)
      
      # Removing the node and the core node from the network
      vertices_to_delete <- c(each_node, each_core_node)
      facebook_graph_without_each_core_node_and_each_node <- delete.vertices(facebook_graph, vertices_to_delete)
      facebook_graph_without_each_core_node_and_each_node$label <- sort(intersect(which(facebook_graph$label != each_core_node), which(facebook_graph$label != each_node)))
      
      disp <- 0
      # If they have 1 mutual friend, distance is 0
      if (length(intersection_of_neighbors) >= 2) {
        combination_of_intersections = combn(intersection_of_neighbors, 2)
        for (index in 1:length(combination_of_intersections)) {
          if (index %% 2) {
            first_node <- combination_of_intersections[index]
            second_node <- combination_of_intersections[index + 1]
            length_of_path <- distances(facebook_graph_without_each_core_node_and_each_node, c(first_node), c(second_node))
            if (length_of_path[1] != Inf) {
              disp <- length_of_path[1] + disp
            }
          }
        }
      }
      
      all_dispersions <- c(all_dispersions, disp)
    }
  }
}

hist(all_embeddednesses, breaks = 50, main = "Distribution of Embeddedness", xlab = "Embeddedness", col="green")
hist(all_dispersions, breaks = 50, main = "Distribution of Dispersion", xlab = "Dispersion", col="green")

picked_core_nodes <- c(1, 2048, 2234)

for (each_core_node in picked_core_nodes) {
  # Create a personal network using the core node
  each_dispersions <- c()
  each_embeddedness <- c()
  neighborhood_of_each_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = each_core_node))
  personal_network_with_each_core_node <- induced.subgraph(facebook_graph, neighborhood_of_each_core_node)
  personal_network_with_each_core_node$label <- sort(neighborhood_of_each_core_node)
  each_core_node_neighbors <- neighbors(facebook_graph, each_core_node)
  
  for (each_node in personal_network_with_each_core_node$label) {
    if (each_core_node != each_node) {
      each_node_neighbors <- neighbors(facebook_graph, each_node)
      intersection_of_neighbors <- intersect(each_core_node_neighbors, each_node_neighbors)
      each_embeddednesses <- c(each_embeddednesses, length(intersection_of_neighbors))
      # Removing the node and the core node from the network
      vertices_to_delete <- c(each_node, each_core_node)
      facebook_graph_without_each_core_node_and_each_node <- delete.vertices(facebook_graph, vertices_to_delete)
      facebook_graph_without_each_core_node_and_each_node$label <- sort(intersect(which(facebook_graph$label != each_core_node), which(facebook_graph$label != each_node)))
      
      disp <- 0
      # If they have 1 mutual friend, distance is 0
      if (length(intersection_of_neighbors) >= 2) {
        combination_of_intersections = combn(intersection_of_neighbors, 2)
        for (index in 1:length(combination_of_intersections)) {
          if (index %% 2) {
            first_node <- combination_of_intersections[index]
            second_node <- combination_of_intersections[index + 1]
            length_of_path <- distances(facebook_graph_without_each_core_node_and_each_node, c(first_node), c(second_node))
            if (length_of_path[1] != Inf) {
              disp <- length_of_path[1] + disp
            }
          }
        }
      }
      
      
      each_dispersions <- c(each_dispersions, disp)
    }
  }
  
  dispersion_over_embeddedness <- dispersion / embeddedness
  
  max_embeddedness <- personal_network$label[which(each_embeddedness == max(each_embeddedness))]
  max_dispersion <- personal_network$label[which(dispersion == max(dispersion))]
  max_dispersion_over_embeddedness <- personal_network$label[which(dispersion_over_embeddedness == max(dispersion_over_embeddedness))]
}