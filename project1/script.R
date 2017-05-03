library(igraph)
library(netrw)

# Import data 
df = read.table("facebook_combined.txt", sep=' ', header = FALSE)
graph = graph.data.frame(df, directed=FALSE)
is.connected(graph)
