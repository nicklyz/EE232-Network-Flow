library(igraph)
library(MASS)
library(ggplot2)
library(Metrics)

# Import data 
g = read.graph("facebook_combined.txt", format="edgelist", directed=FALSE)
is.connected(g)
diameter(g)

g_degree = degree(g)
h = hist(g_degree, breaks = seq(from = min(network_degree), to = max(network_degree), by=1),
         main = "Histogram of Degree of Facebook Graph", xlab = "Degree", ylab = "Frequency")
x = h$mids
y = h$density
smoothSpline = smooth.spline(x, y, spar=0.35)
plot(x,y, main="Degree Distribution of Facebook Network", xlab = "Degree", ylab = "Frequency")
lines(smoothSpline, col = "blue")
cat("Mean Square Error of Fitted Curve")
mse(y, smoothSpline$y)
cat("Average Degree of the Network")
mean(g_degree)
