library(igraph)

# Reading Facebook graph
facebook_graph <- read.graph("facebook_combined.txt", directed=FALSE)
facebook_graph$label <- V(facebook_graph)

# Question 6

# Finding core nodes
core_nodes <- numeric()
for (each_vertex in V(facebook_graph)) {
  if (length(neighbors(facebook_graph, each_vertex)) > 200)
    core_nodes <- c(core_nodes, each_vertex)
}

# We keep statistical results in this table
statistic_res <- matrix(data=list(), nrow=40)
results_data_frame <- data.frame(statistic_res)

# For each core node above
all_core_id <- list()
all_res_density <- list()
all_res_transitivity <- list()
all_res_community_size <- list()

for(i in 1:length(core_nodes)) # consider each core node
{
  #construct personal network (same code from Q3)
  neighborhood_of_core_node <- unlist(neighborhood(facebook_graph, order = 1, nodes = core_nodes[i]))
  personal_network <- induced.subgraph(facebook_graph, neighborhood_of_core_node)
  personal_network$label <- sort(neighborhood_of_core_node)
  
  #get all the communities
  fast_greedy_community <- fastgreedy.community(personal_network)
  
  #for each community in the network
  each_res_density <- vector()
  each_res_transitivity <- vector()
  for(community_index in 1:length(fast_greedy_community))
  {
    community_nodes <- which(fast_greedy_community$membership==community_index)
    community_graph <- induced.subgraph(personal_network, community_nodes)
    
    if (length(community_nodes) > 10)
    {
      each_res_density <- c(each_res_density, graph.density(community_graph))
      each_res_transitivity <- c(each_res_transitivity, transitivity(community_graph))
    }
  }

  all_res_density [[length(all_res_density) + 1]] <- each_res_density
  cat("Density\n")
  print(all_res_density)
  all_res_community_size [[length(all_res_community_size) + 1]]<- as.vector(sizes(fast_greedy_community))
  cat("Community size\n")
  print(all_res_community_size)
  all_res_transitivity [[length(all_res_transitivity) + 1]] <- each_res_transitivity
  cat("Clustering Coefficient\n")
  print(all_res_transitivity)
  all_core_id <- c(all_core_id, i)
}

