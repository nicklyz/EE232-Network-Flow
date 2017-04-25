library(igraph)
library(netrw)

# part(a)
# create undirected random networks with 1000 nodes, with p = 0.01
g <- random.graph.game(1000, 0.01, directed=FALSE)
plot(g)
is.connected(g)

vertex_seq <- netrw(g, 1000, damping = 1, T = 50, output.walk.path = TRUE)$walk.path
vertex_seq[1,50]


random_walk_function = function(num_node, prob) {
  g = random.graph.game(num_node, prob, directed=FALSE)
  cat("Diameter of the random network with ", num_node, "nodes: ", diameter(g))
  average_step_t = numeric()
  for (t in 1:30) {
    distance_vec = numeric()
    vertex_seq_mat = netrw(graph=g, walker.num = num_node, damping=1, T=t, output.walk.path=TRUE)$walk.path
    
    # average over all walkers
    for (i in 1:num_node) {
      start = vertex_seq_mat[1, i]
      end = vertex_seq_mat[t, i]
      # find distance of the walker by using shortest.paths
      distance = shortest.paths(g, start, end)
      
      if (distance == Inf) {
        distance = 0
      }
      # Append the distance to the distance vector
      distance_vec = c(distance_vec, distance)
    }
    average_step_t = c(average_step_t, mean(distance_vec))
  }
  plot(average_step_t, typ='l', main = paste("Average Steps vs. t - ", n, "nodes"), xlab = "t", ylab = "Average Steps")
}


# Problem 1 - Part A & B #
cat("Part a, Random Network with 1000 nodes")
random_walk_function(1000, 0.01)

