library(igraph)

# Part (a)

# Creating an evolving random graph with PA exponent of 1 and age exponent of 1
pa_age_graph <- aging.ba.game(1000, 1, 0, directed = FALSE)
plot(pa_age_graph)

# Plotting the degree distribution
deg_dest <- degree_distribution(pa_age_graph)
plot(deg_dest)

# Part (b)

# Finding the community structure using fast greedy method
community_structure <- fastgreedy.community(pa_age_graph)

# Finding the modularity from the community structure computed with fast greedy method
modularity <- community_structure$modularity
