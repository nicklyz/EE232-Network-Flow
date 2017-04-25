library("igraph")
library("netrw")

# Problem 3(a)

# Creating an undirected random network with 1000 nodes and 0.01 probability of drawing an edge between any pair of nodes
random_network <- erdos.renyi.game(1000, 0.01, directed=FALSE)
# Running netrw to simulate random walk
random_walk_seq <- netrw(random_network, walker.num=1000, damping=1, T=1000, output.walk.path=TRUE)

# Get the degrees of each node in the created network
degree <- degree(random_network)
# Get the probability that the walker visits each node
visit_probability <- random_walk_seq$ave.visit.prob

# Plot degree vs probability that the walker visits each node to examine the relationship
plot(degree, visit_probability)
# Print correlation coefficient
cor(degree, visit_probability)

# Problem 3(b)

# Creating a directed random network with 1000 nodes and 0.01 probability of drawing an edge between any pair of nodes
random_network <- erdos.renyi.game(1000, 0.01, directed=TRUE)
# Running netrw to simulate random walk
random_walk_seq <- netrw(random_network, walker.num=1000, damping=1, T=1000, output.walk.path=TRUE)

# Get the degrees of each node in the created network
degree <- degree(random_network)
# Get the probability that the walker visits each node
visit_probability <- random_walk_seq$ave.visit.prob

# Plot degree vs probability that the walker visits each node to examine the relationship
plot(degree, visit_probability)
# Print correlation coefficient
cor(degree, visit_probability)

# Problem 3(c)

# Creating an undirected random network with 1000 nodes and 0.01 probability of drawing an edge between any pair of nodes
random_network <- erdos.renyi.game(1000, 0.01, directed=FALSE)
# Running netrw with damping parameter 0.85 to simulate random walk
random_walk_seq <- netrw(random_network, walker.num=1000, damping=0.85, T=1000, output.walk.path=TRUE)

# Get the degrees of each node in the created network
degree <- degree(random_network)
# Get the probability that the walker visits each node
visit_probability <- random_walk_seq$ave.visit.prob

# Plot degree vs probability that the walker visits each node to examine the relationship
plot(degree, visit_probability)
# Print correlation coefficient
cor(degree, visit_probability)