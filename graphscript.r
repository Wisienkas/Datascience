# Loading file
p2p_edges <- read.csv2(file = "p2p-Gnutella06.txt", sep = "\t", comment.char = "#")
# Setting headers
names(p2p_edges) <- c("from", "to")

p2p_edges <- p2p_edges[1:1000,]

# Loading library
install.packages("igraph")
install.packages("HiveR")
library(igraph)
library(HiveR)
# Creating graph
graph <- graph.data.frame(p2p_edges)
# Plotting graph
plot(graph)
# Getting betweenness
vertex <- V(graph)
# Watching the structure
head(vertex)
# Betweenness of vertexes
p2p_between_vertex <- betweenness(graph)
# Watching structure
edges <- E(graph)
# Betweennes of edges
p2p_between_edge <- edge_betweenness(graph)
# summary of vertex betweenness
p2p_between_95q <- quantile(p2p_between_vertex, c(0.9995, 1))
# Balance sizes for plot, ignoring everything below mean
bal <- sapply(p2p_between_vertex, FUN = function(x) {
  return(ifelse(x > p2p_between_95q, log(p2p_between_vertex, base = 1.5), 0.01))
})

# Plot grapth based on betweenness
plot(graph, 
     vertex.size = bal,
     vertex.label.cex = 1)

# Mutual connected
p2p_mutual <- reciprocity(graph)

p2p_centrality_eig <- evcent(graph)
bal <- p2p_centrality_eig$vector
plot(graph, 
     vertex.size = bal,
     vertex.label.cex = bal)



p2p_centrality_power <- power_centrality(graph, exponent = 2)
x <- centralization.degree(graph)

weigt_edges <- cbind(p2p_edges, weight = rep(x = 1, times = length(edges)))
# Put into hive object
hive1 <- edge2HPD(edge_df = weigt_edges)
# Mining the edge and radius
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
# Mining for sources, sinks and managers
hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
# Remove nodes going from and to the same node
hive4 <- mineHPD(hive3, option = "remove zero edge")
# Plotting the hive
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)

