library(igraph)

#2a
fat_tail_network <- barabasi.game(1000, directed = FALSE)
deg_ft <- degree(fat_tail_network)
hist(deg_ft, col=rgb(0,1,0,.4), xlim=c(0,50), ylim=c(0,1000), xlab="degree",ylab="freq", main="Fat Tail Degree Distribution")

diameter(fat_tail_network)

#2b
is_connected(fat_tail_network)
cluster <- clusters(fat_tail_network)
giant_component <- induced_subgraph(fat_tail_network, which(cluster$membership == which.max(cluster$csize)))
community_structure <- fastgreedy.community(giant_component)
modularity <- modularity(community_structure)

#2c
large_fat_tail_network <- barabasi.game(10000, directed = FALSE)
large_cluster <- clusters(large_fat_tail_network)
large_giant_component <- induced_subgraph(large_fat_tail_network, which(large_cluster$membership == which.max(large_cluster$csize)))
large_community_structure <- fastgreedy.community(large_giant_component)
large_modularity <- modularity(large_community_structure)

#2d
#repeat the process for 1000 times and collect degree
deg_nj <- numeric()
for (i in 1:1000) {
    #random generate a number within 1000
    n_i <- sample(1000, 1)
    
    #get its neighbor in fat tail network
    neighbor_list <- neighbors(fat_tail_network, n_i)
    
    #if only one neighbor
    if (length(neighbor_list) == 1) {
        n_j <- neighbor_list
    } else {
        n_j <- sample(neighbor_list, 1)
    }
    
    deg_nj <- c(deg_nj, degree(fat_tail_network, n_j))
    
}
hist(deg_nj, col=rgb(0,0,1,.4), xlim=c(0,30), ylim=c(0,600), xlab="degree", ylab="freq", main="Degree Distrubution of jth node")