# R code to accompany Introduction to Network Analysis Workshop
# Melissa Whatley, PhD

# Code based on Gonzalez Canche (2019)

#install.packages("igraph")
#install.packages("RColorBrewer")
#install.packages("classInt")
library(igraph)
library(RColorBrewer)
library(classInt)

# Creating an illustrative edgelist
# Edgelist: A compact representation of a network summarizing connections among nodes
a <- data.frame(from=c("Actor1", "Actor1", "Actor2", "Actor2", "Actor3", "Actor4",
                       "Actor5","Actor6", "Actor7","Actor8","Actor8", "Actor9","Actor10",
                       "Actor11","Actor7"),
                to=c("Actor2", "Actor3", "Actor10", "Actor3", "Actor1", "Actor7",
                     "Actor6","Actor5","Actor10","Actor4","Actor9", "Actor8","Actor3", "Actor1",
                     "Actor1"),
                weight=c(1, 3, 5, 5, 5, 2, 5, 4, 5, 3, 5, 2, 3, 2, 4))
a

# Moving from an edgelist to a graph, we make the third column an attribute, characterizing the strength of the connections
# Storing the dataset as a graph is a necessary step between the edgelist and matrix representation

g<-graph.data.frame(a[,c(1,2,3)])
g

# Getting centrality measures

# Warning messages indicate that more work needs to be done on obtaining closeness centrality measures when networks are relatively sparse - we won't worry about the warning messages for now.
centrality<-data.frame(totaldegree=degree(g, mode="total"),
                       indegree=degree(g,mode="in"),
                       outdegree=degree(g, mode="out"),
                       closeness=round((closeness(g))/max(closeness(g)),3),
                       betweenness=round(betweenness(g, directed=F, normalized = TRUE, weights=NA),3),
                       eigenvector=round(evcent(g,weights=NA)$vector,3))
centrality

# Notes on centrality measures in this example:
# Actor 1 has the highest degree centrality
# Actor 11 has the highest closeness centrality
# Actor 7 has the highest betweenness centrality
# Actor 1 has the highest eigenvector centrality

# If this were an emergency preparation network and I needed to share a piece of information with the network quickly, who would I share the information with first?
# If this were a political network and I wanted to leverage my connections for political influence, who would I want to be my friend?
# If this were a crime network and I wanted to destroy as many connections in the network as possible, who would I remove?

# One use of centrality measures is as variables in regression models:

# Example:
# If I have information on individuals and their occupations merged with closeness centrality measures (and other control variables, such as demographic information), I could analyze the relationship between occupation and closeness centrality. With a large network, identifying a single indivdual for the spread of information might not be very practical, but what if I could identify the occupational category of people with higher average centrality in the network?


# Visualizing these relationships
# We add centrality measures as attributes to the network and can include them in visualizations as colors or sizes. We'll focus on colors today.

# Three color intensities
nclr<-5
# This is the variable I'll use for color intensity
plotvar<-centrality$betweenness
# Choose which color scheme to use
# display.brewer.all()
plotclr<-brewer.pal(nclr, "Greens")
# What cutoff do we use to decide color categories? (here I chose to divide up the values into equal parts (other options include quanitles, standard deviations)
class<-classIntervals(plotvar,nclr, style="equal")

colcode<-findColours(class, plotclr, digits=3)

set.seed(9)

l<-layout.fruchterman.reingold(g, niter=50000)
V(g)$frame<-NA

plot(g, edge.width=E(g)$weight, layout=l, edge.arrow.size=.5, vertex.color=colcode)
title(main="Sociogram Indicating Betweenness Centrality", col.main="black")
legend("bottomleft", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), title="Betweenness Centrality")
