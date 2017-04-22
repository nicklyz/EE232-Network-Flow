library(igraph)

# part(a)
# create undirected random networks with 1000 nodes, with p = 0.01
g <- erdos.renyi.game(1000, 0.01)