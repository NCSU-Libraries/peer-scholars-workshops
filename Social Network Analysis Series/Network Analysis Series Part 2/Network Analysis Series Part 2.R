# R code to accompany Introduction to Network Analysis Workshop
# Melissa Whatley, PhD

# Code based in part on Luke (2015)
# Luke, D.A. (2015). A User's Guide to Network Analysis in R. Springer.

# Edgelist creation (6 students enrolling in 4 classes)

a<-data.frame(student=c("Student1", "Student2", "Student2","Student3","Student3","Student3", "Student4","Student4", "Student5", "Student5","Student6"),
              class=c("Biology", "Biology", "English", "Biology", "English", "Writing","English", "Writing","Writing", "Calculus", "Calculus"))

a

# Step 1: Determine how many credits each student earned and extract other important network information
# First we transform the edgelist into a graph
library(igraph)

library(tibble)

g<-graph.data.frame(a[,c(1,2)])#graph data frame "a" and column 1 to column 2
g 
  plot(g)#you can quickly plot this too

# From the graph, we can extract the matrix (the incidence matrix) we need
v<-as.matrix(get.adjacency(g)) #as a matrix, I want the adjacency as v
v

# Notice that I only need the first six rows and the last four columns:
v<-(v[c(1:6),c(7:10)])
v

# We can use v to calculate the number of classes and resulting number of credits a student earned
attributes<-as.data.frame(v) #v is a matrix, and we want a df now, so change and call attributes
attributes<-cbind(attributes, nclasses=rowSums(attributes))
attributes$ncredits<-attributes$nclasses*3 #from num classes to num credits, assume all 3 cred

# I also want the column with student numbers to have a heading (for future merging purposes)
#library(tibble)
attributes<-rownames_to_column(attributes, var="Student")
attributes

# We can plot the incidence matrix
network<-graph.incidence(v)
plot(network)

# Add colors
colors<-c("blue", "red")
plot(network, vertex.color=colors[V(network)$type+1]) #the V is for vertex, 1 tells where to stop adding blue and start adding red

## Step 2: Determine how many credits a student's peers earned
# Next, we extract the number of credit hours that a student's peers earned
# This first step simply identifies the number of times students were peers
peers<-v%*%t(v) #this will multiply by the transpose
peers #can spot check with incidence matrix to make sure R did it right
diag(peers)<-0 #they're peers with themselves, but we don't care for our network
peers

# We have to graph this matrix so we can extract its edgelist (we turn a two-mode network into a one-mode network - students are now connected to each other THROUGH their classes)
g<-graph.adjacency(peers, mode=c("directed"))
g
plot(g)
edgelist<-as.data.frame(get.edgelist(g))
edgelist

# For our purposes, we don't want to count duplicates - that is, we don't care how many classes students had together. The dplyr package has a nice way of eliminating duplicates
library(dplyr)
edgelist<-edgelist%>%distinct()
edgelist

# I can rename the columns so that they make more sense:

names(edgelist)[names(edgelist) == "V1"] <- "Student"
names(edgelist)[names(edgelist) == "V2"] <- "Peer"

edgelist

# Now that we have an edgelist, we merge in attributes - this time as an attribute of a student's peer, not the student
peers.2<-merge(attributes,edgelist, by.x="Student", by.y="Peer") #by.x is the first df col and by.y is the second df column to use for the merge
peers.2 #creates column Student.y which is their peer

# Only keeping the columns we need:
peers.2<-peers.2[,c(8,1,7)] #arranging columns in from, to, attributes
peers.2

# We extract another graph from this data frame, attaching the peers' number of credits as an attribute
g1<-graph.data.frame(peers.2, directed=TRUE)
g1
plot(g1)

# We can extract the adjacency matrix from this graph to get the number of credits a student's peers took
peers.3<-as.data.frame(as.matrix(get.adjacency(g1, attr="ncredits")))
peers.3

# The row sums in this object are the number of credits a student's peers earned
peers.3<-cbind(peers.3, peerscredits=rowSums(peers.3))
peers.3

## Step 3: Determine how many peers a student has
# The bipartite.projection function in the igraph package extracts both of the possible one-way networks that we could use
network.projection<-bipartite.projection(network)
network.projection

network.student<-network.projection$proj1
network.student

# From this projection, we can extract the matrix that we need to calculate number of peers
matrix.student<-get.adjacency(network.student, sparse=FALSE)
matrix.student

# Convert this matrix into a dataframe and create a column for number of peers
dataframe.student<-as.data.frame(matrix.student)
dataframe.student

dataframe.student<-cbind(dataframe.student, npeers=colSums(dataframe.student))
dataframe.student

# Now we have all the pieces we need:
# The attributes dataset has number of classes and number of credits a student took
# The peers.3 dataset has the number of credits a student's peers earned
# The dataframe.student dataset has the number of peers calculation

# To merge, we need to add the "Student" column header to peers.3 and dataframe.student (because the first col doesn't have a name)

dataframe.student<-rownames_to_column(dataframe.student, var="Student")
dataframe.student

peers.3<-rownames_to_column(peers.3, var="Student")
peers.3

all.data<-merge(dataframe.student, attributes, by="Student")
all.data

all.data<-merge(all.data, peers.3, by="Student")
all.data

## Step 4: Create a peer effects variable

all.data$peereffect<-all.data$peerscredits/all.data$npeers
all.data


## Step 5: Analysis - here, I'll use a simple linear regression model for illustrative purposes

ols<-lm(ncredits~peereffect, data=all.data)
summary(ols)
