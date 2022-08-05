#########################################################
#
# Author Nick Lichtsinn
#Lab 8: class social network data (stucture and cleaning)
#
#########################################################


my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 8/"
link.data <- read.csv(paste0(my.dir, "LINKS-421-719Network.csv"), header = TRUE, stringsAsFactors = FALSE)
node.data <- read.csv(paste0(my.dir, "NODES-421-719Network.csv"), header = TRUE, stringsAsFactors = FALSE)

library(igraph)

View(node.data)
View(link.data)

dim(node.data)

# clean the data
colnames(link.data) <- gsub("\\.", "", colnames(link.data))
link.data$X <- gsub(" |-", "", link.data$X)
cbind(link.data$X, colnames(link.data)[-1])

node.data$Name <- gsub(" |-", "", node.data$Name)
cbind(node.data$Name, link.data$X)

M <- as.matrix(link.data[ , -1])
rownames(M) <- colnames(M)
dim(M)

any(is.na(M))
M[is.na(M)] <- 0
M[M > 1]

g <- graph_from_adjacency_matrix(M)

#########################################################
#
# The graph object and the first plot
#
#########################################################

vcount(g) # tells how many nodes there are
ecount(g) # edges count

plot.igraph(g)

g <- simplify(g) # suppress self loops
par(mar = c(0,0,0,0))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

E(g)$arrow.size <- 0
E(g)$arrow.width <- 0
plot.igraph(g)

V(g)$color <- "gold"
V(g)$frame.color <- "white"
V(g)$label.color <- "black"
E(g)$color <- "cadetblue"
V(g)$size <- 5
plot.igraph(g)

?igraph.plotting()

E(g)$curved <- .4
plot.igraph(g)

#########################################################
#
# Visualizing Centrality and Centrality Measurements
#
#########################################################

# degree centrality
degree(g) # counts how many links each person has
par(mar = c(3,10,1,1))
barplot(sort(degree(g)), horiz = TRUE, las = 2, main = "Most Popular")

V(g)$degree <- degree(g)
V(g)$deg.out <- degree(g, mode = "out")
V(g)$deg.in <- degree(g, mode = "in")

barplot(sort(V(g)$deg.out), names.arg = V(g)$name, horiz = TRUE, las = 2, main = "Most Friendly")

barplot(V(g)$deg.in, names.arg = V(g)$name, horiz = TRUE, las = 2, main = "Most In")

# closeness
V(g)$close <- closeness(g, normalized = TRUE, mode = "all")
V(g)$bet <- betweenness(g, directed = FALSE)

library(plotrix)
my.pallet <- colorRampPalette(c("steelblue1", "violet", "tomato", "red", "red")) # create pallet
V(g)$color <- rev(my.pallet(200))[round(1+rescale(V(g)$close, c(1,199)), 0)] # set color based on closeness
plot.igraph(g)

V(g)$size <- 2 + rescale(V(g)$degree, c(0,13))
V(g)$label.cex <- .7 + rescale(V(g)$bet, c(0, 1.25))

plot.igraph(g)

#########################################################
#
# Visualizing Social Network Structures
#
#########################################################

cbind(node.data$Name, link.data$X)

V(g)$class <- node.data$Class
V(g)$country <- node.data$Country
V(g)$year <- node.data$year

g <- delete.vertices(g, "JoHunter")
plot.igraph(g, main = "Network!")

V(g)$shape <- "circle"
V(g)$shape[V(g)$class == "Wednesday"] <- "square"
V(g)$shape[V(g)$class == "Both"] <- "rectangle"

V(g)$color <- "gold"
V(g)$color[V(g)$country == "India"] <- "springgreen4"
V(g)$color[V(g)$country == "China"] <- "red"
V(g)$color[V(g)$class == "Both"] <- "purple"

V(g)$label.color <- "blue"
V(g)$label.color[V(g)$year == 1] <- "black"

fc <- cluster_fast_greedy(as.undirected(g))
print(modularity(fc))

membership(fc)
v(g)$cluster <- membership(fc)
length(fc) # 4 different clusters in this network
sizes(fc) # relative sizes of each group

par(mar = c(0,0,0,0))


plot_dendrogram(fc, palette = rainbow(7))
  
plot.igraph(g)

#########################################################
#
# Visualizing Social Network Structures 
#    using ist719NetworkObject.rda
#
#########################################################
load(paste0(my.dir, "ist719NetworkObject.rda"))
par(mar = c(0,0,0,0))
plot.igraph(g)

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]

l <- layout_with_fr(g)

E(g)$color <- "gray"
E(g)[from("LeelaDeshmukh")]$color <- "red"

l <- layout_as_star(g, center = "LeelaDeshmukh")

l <- layout_with_kk(g)

V(g)$x <- 0
V(g)$y <- 0
plot.igraph(g)
coord <- cbind(V(g)$x, V(g)$y)

iteration <- c(500, 100, 20, 10, 5, 3, 2, 1)
for (i in 1:length(iteration)) {
  l <- layout_with_fr(g, coords = coord, dim = 2, niter = iteration[i])
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  plot.igraph(g)
  mtext(paste("Layout FR:", iteration[i]), side = 3, line = 0, cex = 1.5, adj = 0)
}

l <- layout_with_gem(g)
l <- layout_with_dh(g)
l <- layout_on_grid(g)

my.linked.list <- data.frame(person = V(g)$name, event = V(g)$country)
g <- graph_from_data_frame(my.linked.list, directed = FALSE)
g

V(g)$type <- FALSE
V(g)$type[V(g)$name %in% node.data$Name] <- TRUE

l <- layout_as_bipartite(g, types = V(g)$type)
V(g)$x <- l[, 2]
V(g)$y <- l[, 1]

par(mar = c(0,0,0,0))
plot.igraph(g)

V(g)$size <- 0



































































