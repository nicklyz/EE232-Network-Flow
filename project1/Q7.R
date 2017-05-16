library(igraph)

file_names = list.files("gplus/")
file_ids = sub("^([^.]*).*", "\\1", file_names)
ego_node_ids = unique(file_ids)
# should have 132 ego nodes

# extract personal network who have more than 2 circles
circle_ids = numeric()
for (id in ego_node_ids) {
  circle_filename = paste("gplus/", id, ".circles", sep="")
  circle_file = file(circle_filename, open="r")
  circle = readLines(circle_file)
  close(circle_file)
  
  if (length(circle) > 2) {
    circle_ids = c(circle_ids, id)
  }
}
# should have 57 networks left


