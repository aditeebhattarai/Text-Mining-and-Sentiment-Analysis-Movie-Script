#########################################################################
#Group 2 MIS 612 Harry Potter Term Project
#Harry Potter and the Chamber of Secrets
#########################################################################

#Install Packages
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("tidytext")
#install.packages("RWeka")
#install.packages("reshape2")
#install.packages("radarchart")
#install.packages("circlize")
#install.packages("dplyr")
#install.packages("dbplyr")
#install.packages("igraph")
#install.packages("Matrix")
#install.packages("RColorBrewer")

#Load Packages
library(dplyr)
library(dbplyr)
library(tm)
library(textstem)
library(igraph)
library(Matrix)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(RWeka)
library(reshape2)
library(radarchart)
library(circlize)
library(RColorBrewer)

#######################################################################
#SNA Social network Creation
#######################################################################

#Input the CSV script
HPScript<-read.csv("HPScript.csv")

#List the Speakers and Listeners
Speak<-HPScript[,c("Speaker","Listener")]

#Remove Blank Charactes
Speak<-Speak%>% filter(Listener != "")
Speak<-Speak%>% filter(Speaker != "")

#Count number of Speakers
length(unique(Speak$Speaker))
unique(Speak$Speaker)

#Count number of Listeners
length(unique(Speak$Listener))
unique(Speak$Listener)

#Count number of times the characters speak
Dialogue<-Speak %>% group_by(Speaker, Listener) %>% summarise(counts = n())

#Create a vector of character
speakers<-c(as.character(Dialogue$Speaker), as.character(Dialogue$Listener))
speakers<-unique(speakers)

#Assign values to Matrix with nodes (speakers) connected through edges (dialgoue)
graph<-graph_from_data_frame(d=Dialogue, vertices=speakers, directed=TRUE)
graph

#View Speakers as nodes
V(graph)$name

#View Dialogue as edges
E(graph)

#Plot graph for SNA
set.seed(1)
plot(graph, vertex.label.color = "black")

#Assign weights based on number of conversations
weights<-E(graph)$counts

#Apply weights
set.seed(1)
plot(graph, 
     vertex.label.color = "black", 
     edge.color = 'White',
     edge.width = sqrt(weights),  
     layout = layout_nicely(graph))

#Delete the edges with weight < 2
graph2<-delete_edges(graph,E(graph)[weights<2])
plot(graph2,vertex.label.color="black",edge.width=10,vertex.size=4)

#Calculate Betweeness
g.b<-betweenness(graph2)

#Plot the vertex size using betweenness score
plot(graph2,vertex.label.color="black",
     edge.color="red",
     vertex.size=sqrt(g.b),
     edge.arrow.size=0.008,
     layout=layout_nicely(graph2))

#Simplify the SNA
graph2<-simplify(graph2)
g.b[order(g.b, decreasing = TRUE)][1:20]

#Plot a Simplified Graph
plot(graph2,vertex.label.color="black",
     edge.color="black",
     vertex.size=sqrt(g.b),
     edge.arrow.size=0.008,
     layout=layout_nicely(graph2))


