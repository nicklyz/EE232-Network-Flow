library(igraph)
library(netrw)

# reusable function for creating random network with different size
random_walk_function = function(num_node, num_step) {
  g = barabasi.game(num_node, directed=FALSE)
  cat("Diameter of the random network with", num_node, "nodes:", diameter(g))
  average_distance_t = numeric()
  average_standard_deviation_t = numeric()
  deg_random_walk_end = numeric()
  for (t in 1:num_step) {
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
      if (t == num_step) {
        deg_random_walk_end = c(deg_random_walk_end, degree(g, v = end))
      }
    }
    average_distance_t = c(average_distance_t, mean(distance_vec))
    average_standard_deviation_t = c(average_standard_deviation_t, mean((distance_vec - mean(distance_vec))**2))
  }
  plot(average_distance_t, typ='l', main = paste("Average Distance vs. t -", num_node, "nodes"), xlab = "Steps", ylab = "Average Distance")
  plot(average_standard_deviation_t, typ='l', main = paste("Average Standard Deviation vs. t - ", num_node, "nodes"), xlab = "Steps", ylab = "Average Standard Deviation")
  
  # part (e)
  if (num_node == 1000) {
    g_deg = degree(g)
    hist(x = g_deg, main = "Degree Distribution for Random Graph (n=1000)", xlab = "Number of Degrees")
    hist(x = deg_random_walk_end, main = "Degree Distribution at end of Random Walks", xlab = "Number of Degrees") 
  }
}


# part (a) and part (b)
cat("Part a & b, Random Network with 1000 nodes")
random_walk_function(1000, 60)

# part (d)
cat("Part d, Random Network with 100 nodes")
random_walk_function(100, 40)
cat("Part d, Random Network with 10000 nodes")
random_walk_function(10000, 40)
