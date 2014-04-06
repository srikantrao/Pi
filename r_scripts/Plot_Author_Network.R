# Creating a NanoHub Author Network using Tool Author dataset 

# Author - Srikant Rao

# Set to correct working directory
setwd("C:/Users/Shrikant Rao/Documents/Nanohub/R Scripts/") 


#----------------------------Building the Data Frame--------------------

# Assumes that author_subset is already creating containing the author subset

library(igraph)

# Create graph using rowvalues in dataframe 

author_subset[author_subset== 0] <- NA
library(igraph)
author_network<-graph.data.frame(d = author_subset,directed=FALSE)
author_network<- delete.vertices(author_network, which(degree(author_network)== 106))
plot(author_network,vertex.size = 0.03,vertex.label=NA)

par(mai=c(0,0,1,0)) 			#Specify size of margins
plot(author_network,			#the graph to be plotted
layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
main='NanoHub Tool Author Network',	#specifies the title
vertex.label.dist=0.5,			#puts the name labels slightly off the dots
vertex.frame.color='blue', 		#the color of the border of the dots 
vertex.label.color='black',		#the color of the name labels
vertex.label.font=0.5,			#the font of the name labels
vertex.label=V(author_network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
vertex.label.cex=0.25,			#specifies the size of the font of the labels. can also be made to vary
vertex.size = 0.75                   # Reduce the size of the nodes
)

wt<- walktrap.community(author_network, modularity=TRUE)
dend<- as.dendrogram(author_network, use.modularity=TRUE)
plot(dend, nodePar=list(pch=c(NA, 20)))

#-----------------Calculating Centrality Measures------------------------

author_deg_cent<-degree(author_network)
author_closeness_cent<-closeness(author_network)