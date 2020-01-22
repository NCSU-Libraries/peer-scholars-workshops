# R code to accompany "A Gentle Introduction to Network Analysis in R" (Practice)
# Melissa Whatley, PhD

# 1. Set the working directory
setwd("/Users/melwhat/Dropbox/Teaching Materials/Network Analysis/Introduction to Network Analysis")
getwd()

# 2. Load the packages you need
library(igraph)
library(RColorBrewer)
library(classInt)

# 3. Load the .csv containing the advice network edgelist

a <- read.csv("edgelist.csv")
a

# 4. Store the edgelist as a graph, then store the graph as a matrix

g<-graph.data.frame(a[,c(1,2,3)])
g

v<-as.matrix(get.adjacency(g, attr="weight"))
v

# 5. Get centrality measures for all the actors in the advice network

centrality<-data.frame(totaldegree=degree(g, mode="total"),
                       indegree=degree(g,mode="in"),
                       outdegree=degree(g, mode="out"),
                       closeness=round((closeness(g))/max(closeness(g)),3),
                       betweenness=round(betweenness(g, directed=F, normalized = TRUE, weights=NA),3),
                       eigenvector=round(evcent(g,weights=NA)$vector,3))
centrality

# What do these measures tell us about the role of these actors in the network?

# In-degree: Rachel and John provide advice the most
# Out-degree: Rachel and Jennifer ask for advice the most
# Closeness: Rob has the highest closeness centrality. If I wanted to share a piece of information with the network, I'd choose him.
# Betweeness: Rachel has the highest betweenness centrality, so she can pass advice between people most effectively.
# Eigenvector: Rachel also has the highest eigenvector centrality. She's connected to the most prominent advice givers.

# 6. Create a visualization of the network that illustrates how individuals act as “bridges” within the network. Make the nodes of your network blue.

# "Bridges" within the network are indicative of betweenness centrality.

# Three color intensities
nclr<-3
plotvar<-centrality$betweenness
plotclr<-brewer.pal(nclr, "Blues")
class<-classIntervals(plotvar,nclr, style="equal")
colcode<-findColours(class, plotclr, digits=3)

set.seed(9)

l<-layout.fruchterman.reingold(g, niter=50000)
V(g)$frame<-NA

plot(g, edge.width=E(g)$weight, layout=l, edge.arrow.size=.5, vertex.color=colcode)
title(main="Sociogram Indicating Betweenness Centrality", col.main="black")
legend("topleft", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), title="Betweenness Centrality")