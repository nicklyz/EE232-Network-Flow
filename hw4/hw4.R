library(igraph)
setwd("./Documents/EE 232E/EE232-Network-Flow/hw4/")

# Question 1

read_stock_info = function(csv_name) {
  return(read.csv(paste("data", csv_name, sep="/")))
}

get_time_series_from_stock_info = function(csv_name) {
  # Get the stock info from a csv file
  stock_info = read_stock_info(csv_name)
  # Convert into a time series
  
  # 2 of the csv files are mis-formatted: "BF.B.csv", "BRK.B.csv"
  if (csv_name == "BF.B.csv" || csv_name == "BRK.B.csv")
    return(xts(stock_info[,-1], order.by = as.Date(stock_info$Date, format="%m/%d/%y")) )
  return(xts(stock_info[,-1], order.by = as.Date(stock_info$Date)))
}

# Computes r_i(t)
r_t = function(time_series) {
  closing_prices = time_series$Close
  shifted_closing_prices = lag(time_series$Close, 1)
  return((log(closing_prices) - log(shifted_closing_prices))[-1])
}

# Computes the cross correlation coefficient
cross_correlation_coefficient = function(time_series1, time_series2) {
  r1_t = r_t(time_series1)
  r2_t = r_t(time_series2)
  
  top = mean(r1_t * r2_t) - mean(r1_t) * mean(r2_t)
  bot = (mean(r1_t^2) - mean(r1_t)^2) * (mean(r2_t^2) - mean(r2_t)^2)
  return(top / sqrt(bot))
}


# Question 2

# Get all the file csv names
all_csv_file_names = list.files(paste("data", sep="/"), pattern="*.csv")
all_time_series = list()

# Getting all time series from list of csv files so that we can only compute it once
for (csv_file_name in all_csv_file_names) {
  all_time_series[[csv_file_name]] = get_time_series_from_stock_info(csv_file_name)
}

# Creating a cross correlation coefficient matrix
cross_correlation_coefficient_matrix = matrix(0, nrow = length(all_csv_file_names), ncol = length(all_csv_file_names))
i = 1
for (csv_file_name1 in all_csv_file_names) {
  j = 1
  for (csv_file_name2 in all_csv_file_names) {
    if (csv_file_name1 != csv_file_name2) {
      time_series1 = all_time_series[[csv_file_name1]]
      time_series2 = all_time_series[[csv_file_name2]]
      cross_correlation_coefficient_matrix[i, j] = cross_correlation_coefficient(time_series1, time_series2)
    }
    j = j+1
  }
  i = i+1
}

# Computing D
D = sqrt(2 * (1 - cross_correlation_coefficient_matrix))
hist(D, main = "Histogram of d", xlab = "d", col="light blue")

# Constructing a correlation graph
G = graph_from_adjacency_matrix(D, weighted=TRUE, mode="undirected")

# Question 3
minst = mst(G, weights = E(G)$weight)
all_sectors = read.csv("Name_sector.csv")
symbol_to_sector = list()
for (index in 1:length(all_sectors$Symbol)) {
  symbol_to_sector[[all_sectors$Symbol[index]]] = all_sectors$Sector[index]
}
unique_sectors = unique(read.csv("Name_sector.csv")$Sector)
color_per_sector = list()
i = 1

for (sector in unique_sectors) {
  color_per_sector[[sector]] = i
  i = i + 1
}

V(G)$symbol = all_sectors$Symbol
V(minst)$symbol = all_sectors$Symbol
V(minst)$color = unlist(lapply(V(minst)$symbol, function(x) {color_per_sector[[symbol_to_sector[[x]]]]}))

plot(minst, vertex.size = 5, edge.arrow.size=0, vertex.color = V(minst)$color, vertex.label=NA)

# Question 4
sum_of_p = 0
for (vertex in V(minst)) {
  neighbors_of_vertex = neighbors(minst, vertex)
  sector_of_vertex = V(minst)[vertex]$color
  top_p = 0
  bot_p = length(neighbors_of_vertex)
  for (neighbor in neighbors_of_vertex) {
    if (sector_of_vertex == V(minst)[neighbor]$color) {
      top_p = top_p + 1
    }
  }
  sum_of_p = sum_of_p + (top_p / bot_p)
}

alpha1 = sum_of_p / vcount(minst)

sampling_probability = unlist(table(V(minst)$color) / vcount(G))
V(minst)$color = sample(1:11, vcount(minst), replace=TRUE, prob = sampling_probability)

sum_of_p = 0
for (vertex in V(minst)) {
  neighbors_of_vertex = neighbors(minst, vertex)
  sector_of_vertex = V(minst)[vertex]$color
  top_p = 0
  bot_p = length(neighbors_of_vertex)
  for (neighbor in neighbors_of_vertex) {
    if (sector_of_vertex == V(minst)[neighbor]$color) {
      top_p = top_p + 1
    }
  }
  sum_of_p = sum_of_p + (top_p / bot_p)
}

alpha2 = sum_of_p / vcount(minst)

# Question 5
minst = mst(G, weights = E(G)$weight)
V(minst)$symbol = all_sectors$Symbol
V(minst)$color = unlist(lapply(V(minst)$symbol, function(x) {color_per_sector[[symbol_to_sector[[x]]]]}))

# Using simple approximation method of w(MST) < w(TSP) < 2 w(MST)
# w(MST)
sum(E(minst)$weight)
# 2*w(MST)
sum(E(minst)$weight) * 2

# Checking for Triangle Inequality
for (vertex1 in V(G)) {
  for (vertex2 in V(G)) {
    for (vertex3 in V(G)) {
      if (vertex1 != vertex2 && vertex2 != vertex3 && vertex1 != vertex3){
        if (D[vertex1, vertex2] > (D[vertex1, vertex3] + D[vertex2, vertex3])) {
          print("FALSE")
        }
      }
    }
  }
}
# Didn't print false so Triangle Inequality holds

# Creating a multi-graph
multi_graph = make_empty_graph(505, directed = FALSE)
index = 0
for (vertex in V(minst)) {
  neighbors_of_vertex = neighbors(minst, vertex)
  for (each_neighbor in neighbors_of_vertex) {
    multi_graph = multi_graph + edge(vertex, each_neighbor)
    index = index + 1
  }
}
E(multi_graph)$weight = E(minst)$weight

ep = eulerian(multi_graph)
# 609.298086027

# Using TSP Library
# 498.033210104334

# Question 6

