library(igraph)

#1a
#Create three undirected random networks (n = 1000, p = 0.01, 0.05, 0.1)
random_network1 <- erdos.renyi.game(1000, 0.01, type="gnp")
random_network2 <- erdos.renyi.game(1000, 0.05, type="gnp")
random_network3 <- erdos.renyi.game(1000, 0.1, type="gnp")

#Plot degree distributions for these three networks
deg1 <- degree(random_network1)
deg2 <- degree(random_network2)
deg3 <- degree(random_network3)
hist(deg1, col=rgb(0,0,1,.4), xlim=c(0,30), xlab="degree",ylab="freq", main="p=0.01")
hist(deg2, col=rgb(1,0,0,.4), xlim=c(0,100), xlab="degree",ylab="freq", main="p=0.05")
hist(deg3, col=rgb(0,1,0,.4), xlim=c(0,200), xlab="degree",ylab="freq", main="p=0.1")

#1b
#is the network connected or disconnected
is_connected(random_network1)
is_connected(random_network2)
is_connected(random_network3)

#diameters
diameter(random_network1)
diameter(random_network2)
diameter(random_network3)

#1c
threshold_prob <- 0.001
random_network4 <- erdos.renyi.game(1000, threshold_prob, type="gnp")
while (!(is_connected(random_network4)))
{
    threshold_prob <- threshold_prob + 0.001
    random_network4 <- erdos.renyi.game(1000, threshold_prob, type="gnp")
}

#1d See report