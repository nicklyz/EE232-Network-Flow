library(igraph)
library(netrw)

#Q4

#part a
random_network <- erdos.renyi.game(1000, 0.01, type="gnp", directed=TRUE)
page_rank <- netrw(random_network, walker.num=1000, start.node=sample(1:vcount(random_network)), damping = 0.85, T=1000, output.walk.path = TRUE)
plot(density(page_rank$ave.visit.prob), xlim=range(0,0.003), ylim=range(0,1400),xlab = "probability", ylab = "density")

#part b
tel_p = page_rank$ave.visit.prob
p_page_rank = netrw(random_network, walker.num = 1000, start.node=sample(1:vcount(random_network)), damping = 0.85, T = 1000, teleport.prob = tel_p, output.walk.path = TRUE)
#compare results
#plot two graphs in the same plot
par(new=TRUE)
plot(density(p_page_rank$ave.visit.prob), col = "red", axes = FALSE,xlim=range(0,0.003), ylim=range(0,1400), xlab = "", ylab = "", main = "")

