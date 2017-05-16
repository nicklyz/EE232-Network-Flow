library(igraph)

egoNodes <- dir("gplus",pattern = "circles")
# should have 132 ego nodes

general_filenames <- list()
for (i in 1:length(egoNodes))
{
  general_filenames[[i]] <- strsplit(egoNodes[i],".circles")
  circles_file <- paste("gplus/", general_filenames[[i]], ".circles", sep="")
  file_connection <- file(circles_file , open="r")
  circles <- readLines(file_connection)
  close(file_connection)
  
  # check if more than 2 circles
  if(length(circles) > 2)
  {
    all_circles <- list()
    for (c in 1:length(circles)) 
    {
      content <- strsplit(circles[c],"\t")
      all_circles <- c(all_circles, list(content[[1]][-1]))
    }
  
    edges_file <- paste("gplus/", general_filenames[[i]], ".edges", sep="")
    google_network <- read.graph(edges_file, format = "ncol", directed=TRUE)
    google_network <- add.vertices(google_network, nv = 1, name = general_filenames[[i]])
    
    edge_list <- c()
    for (node in 1:(vcount(google_network)-1))
    {
      edge_list <- c(edge_list, c(vcount(google_network), node))
    }
    
    google_network <- add_edges(google_network, edge_list)
    
    info_map_community <- infomap.community(google_network)
    walk_trap_community <- walktrap.community(google_network)
    
    info_map_overlap_ratio <- vector()
    walk_trap_overlap_ratio <- vector()
    for(x in 1:max(info_map_community$membership))
    {
      info_map_community_nodes = V(google_network)$name[which(info_map_community$membership == x)]
      
      for (c in 1:length(all_circles))
      {
        union_nodes <- length(union(info_map_community_nodes, all_circles[[c]]))
        info_map_overlap_ratio = c(info_map_overlap_ratio, length(intersect(info_map_community_nodes, all_circles[[c]]))/union_nodes)
      }
    }
    
    for(x in 1:max(walk_trap_community$membership))
    {
      walk_trap_community_nodes = V(google_network)$name[which(walk_trap_community$membership == x)]
      
      for (c in 1:length(all_circles))
      {
        union_nodes <- length(union(walk_trap_community_nodes, all_circles[[c]]))
        walk_trap_overlap_ratio = c(walk_trap_overlap_ratio, length(intersect(walk_trap_community_nodes, all_circles[[c]]))/union_nodes)
      }
    }

    print(paste("Number of Circles: ", length(all_circles)))
    print(paste("Max for walktrap: ",max(walk_trap_overlap_ratio)))
    print(paste("Max for infomap: ",max(info_map_overlap_ratio)))
  
  }
}

