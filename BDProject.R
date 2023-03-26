# Social Network Analysis
library(igraph)
#importing the file and reading it
data <- read.csv(file.choose(), header=T)
y <- data.frame(data$first, data$second)

# Creating network
net <- graph.data.frame(y, directed=T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# Histogram of node degree
hist(V(net)$degree,
     col = 'pink',
     main = 'Histogram for Node Degree',
     ylab = 'Frequency',
     xlab = 'The Degree of Vertices')

#The Network diagram
set.seed(222)
plot(net,
     vertex.color = 'pink',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

# For Highlighting degrees & layouts
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)

# For Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(51),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
set.seed(123)
plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(51),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))

# For Community detection
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)
