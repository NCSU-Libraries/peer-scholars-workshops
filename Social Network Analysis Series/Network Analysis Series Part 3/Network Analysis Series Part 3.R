# R code to accompany Introduction to Network Analysis Workshop (Part 3)
# Melissa Whatley, PhD

rm(list=ls()) #clears your memory in R, so you start fresh
# setwd("/Users/melwhat/Dropbox/Teaching/Network Analysis/Network Analysis Series Part 3")
getwd()

library(igraph)

# Part 1: Load a dataset with participants linked to qualitative codes
data<-read.csv("Qualitative Content Codes Example.csv")
head(data)

# Graph of the data - just to take a look
g<-graph.data.frame(data[,c(1,2)]) #graph columns 1 and 2
g

# You can play with different layouts to make your graph look nicer (? layout)
l<-layout_with_graphopt(g) #go to ?layout to see the different layout options and their algorithms

plot(g)

plot(g, layout=l) #gets you mostly there, try setting the seed to play with node positions, or different layouts

# If we want to get fancier, we can add colors and shapes to the vertices to identify what they are (participants or qualitative codes)

# I know we have 15 participants

# What is V(g)

V(g)

V(g)$shape<-"square"
V(g)$shape[1:15]<-"circle"
V(g)$color<-"grey"
V(g)$color[1:15]<- "green"

plot(g, layout=l)

# Part 2: What can we do with this graph?
# If we make it a one-mode network, we can get centrality measures for participants or for qualitative codes

v<-as.matrix(get.adjacency(g))
v

v<-(v[c(1:15),c(16:21)])
v

# A one-mode network of participants:
connections<-v%*%t(v)
connections
diag(connections)<-0
connections

g<-graph.adjacency(connections, mode=c("directed"))
g
l<-layout_with_fr(g)
plot(g, layout=l, edge.arrow.size=1)
plot(g, layout=l, edge.arrow.size=.5)


centrality<-data.frame(totaldegree=degree(g, mode="total"),
                       indegree=degree(g,mode="in"),
                       outdegree=degree(g, mode="out"),
                       closeness=round((closeness(g))/max(closeness(g)),3),
                       betweenness=round(betweenness(g, directed=F, normalized = TRUE, weights=NA),3),
                       eigenvector=round(evcent(g,weights=NA)$vector,3))
centrality

# Example: Participant 10 has the highest degree centrality , so this participant shared the most reasons for workshop participation with other participants. Participant 6 had the lowest degree centrality, so this participant shared the fewest reasons for participation with other participants. (one use of these measures might be as predictor variables in a regression analysis)

# A one-mode network of qualitative codes:
connections.2<-t(v)%*%v
connections.2
diag(connections.2)<-0
connections.2

g<-graph.adjacency(connections.2, mode=c("directed"))
g
l<-layout_with_fr(g)
plot(g, layout=l, edge.arrow.size=1)
plot(g, layout=l, edge.arrow.size=.5)


centrality<-data.frame(totaldegree=degree(g, mode="total"),
                       indegree=degree(g,mode="in"),
                       outdegree=degree(g, mode="out"),
                       closeness=round((closeness(g))/max(closeness(g)),3),
                       betweenness=round(betweenness(g, directed=F, normalized = TRUE, weights=NA),3),
                       eigenvector=round(evcent(g,weights=NA)$vector,3))
centrality

# Example: Utility_Applicability connected the most participants (total degree centrality) and also had the highest eigenvector centrality, so this code was also connected to other important codes in the network.

# Part 3: Community Detection
# We'll detect communities among the qualitative codes

g<-graph.data.frame(data[,c(1,2)])
g

# Other algorithms might be more useful for your purposes (see ? cluster_)
# A spinglass algorithm detects a set of nodes with many edges inside the community and few edges outside of it

communities <- cluster_spinglass(g)
membership(communities)

communities <- cluster_spinglass(g, spins=2)
membership(communities)

# Community detection can be used to validate pre-existing groups that you thing are in your dataset.

# Part 4: Quadratic Assignment Procedure (QAP)
# We'll explore whether the semantic connections of STEM workshop participants are different from those of non-STEM workshop participants

library(sna)

head(data)

# Remember the connections object is the number of codes that participants have in common

connections

# To compare networks, we need another matrix that tells us if participants are in the same group or not (STEM or non-STEM)

data2<-read.csv("Qualitative Content Codes Example.csv")
dim(data2)

data2<-(data2[,c(1,3)])
head(data2)

library(dplyr)
data2<-data2%>%distinct()
data2

g<-graph.data.frame(data2)
g

v3<-as.matrix(get.adjacency(g))
dim(v3)
v3

v4<-v3[1:15,16:17]
v4t<-v4%*%t(v4)
diag(v4t)<-0
dim(v4t)
v4t

dim(v4t)
dim(connections)

#Both matrices in a single object
g<-array(dim=c(2,15,15))
g
#Adding the matrices
g[1,,]<-connections #Number of codes in common
g[2,,]<-v4t #Whether in same STEM group
dim(g)

# A quadratic assignment procedure estimates the extent to which dyads in two groups are correlated with one another (gcor) (e.g., if Particpant 1 and Participant 2 share content codes, do they also share a field of study?).
# QAP scrambles the dependent variable (shared codes) a certain number of times ('reps=') and estimates correlations between dyads that number of times, effectively creating an empirical sampling distribution that can be used to estimate standard errors.
# We compare the real correlation among dyads to the sampling distribution to determine if the two networks are related to one another

qap<-qaptest(g,gcor,g1=1,g2=2,reps=100)
summary(qap)
plot(qap)

qap<-qaptest(g,gcor,g1=1,g2=2,reps=5000)
summary(qap)
plot(qap)

qap<-qaptest(g,gcor,g1=1,g2=2,reps=10000)
summary(qap)
plot(qap)

# We can also correlate the matrix to itself to see what it looks like when there's no significant difference
qap<-qaptest(g,gcor,g1=1,g2=1,reps=5000)
summary(qap)
plot(qap)





