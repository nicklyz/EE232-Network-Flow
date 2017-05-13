library(igraph)

g = read.graph("facebook_combined.txt", directed=FALSE)
neighborhood = neighborhood(g, order=1, nodes=1)
personal_network = induced.subgraph(g, vids=unlist(neighborhood), impl="auto")
personal_network$label = sort(unlist(neighborhood))

num_nodes = vcount(personal_network)
num_edges = ecount(personal_network)

cat("Number of Nodes in Personal Network:" , num_nodes)
cat("Number of Edges in Personal Network:" , num_edges)

node_size = rep(2, num_nodes)
node_size[personal_network$label == 1] = 4
node_color = rep("lightblue", num_nodes)
node_color[personal_network$label == 1] = "black"
plot(personal_network, vertex.label=NA, vertex.size=node_size, 
     vertex.color=node_color, asp=9/16)
