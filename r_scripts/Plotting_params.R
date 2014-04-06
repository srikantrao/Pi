setwd("/home/srikant/My Documents/Nanohub/")
edge_attrib<-read.csv("List2.csv")

# Remove the first entry 
edge_attrib<-edge_attrib[-1,]

# Remove all the null entries 
edge_attrib<-(edge_attrib[!((edge_attrib$TO=="")|(edge_attrib$FROM=="")), ]) # 10318 free entries in the list

edge_weight<-as.numeric(edge_attrib[,3])

library("igraph") # Look at statnet for additional features 
# Graphing using adjacency matrix
#dev_graph<-graph.adjacency(dev_matrix,mode="directed",weighted=TRUE)
edge_list=as.matrix(edge_attrib)
dev_graph=graph.edgelist(edge_list[,2:3],directed=TRUE)
E(dev_graph)$weight=as.numeric(edge_list[,4])


#dev_graph <- graph.data.frame(edge_attrib, directed=TRUE, vertices=vertex_attrib)
#plotting the graph 

# Plotting parameters 
V(dev_graph)$size = degree(dev_graph)*0.05
V(dev_graph)$color = c("grey","brown","violet")
V(dev_graph)[size>7]$color="red"
V(dev_graph)[size<7 & size>5]$color="blue"
V(dev_graph)[size<5 & size>2]$color="purple"
V(dev_graph)[size<2 & size>1]$color="yellow"
V(dev_graph)[size<1&size>0.5]$color="cyan"
V(dev_graph)[size<1&size>0.5]$color="green"
par(mai=c(0,0,1,0))       #Specify size of margins
plot(dev_graph,			#the graph to be plotted
     layout=layout.lgl,	# the layout method. see the igraph documentation for details
     main=' Developer Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=100,			#the font of the name labels
     vertex.label=NA,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.25,		#specifies the size of the font of the labels. can also be made to vary                               # Reduce the size of the nodes
     edge.arrow.size = 0.02,
     edge.width = E(dev_graph)*0.0000001 # set the 
)

# 3D plot -- currently useless .. need to find 3D layout and add all the nodes for better viewing 
rglplot(dev_graph,layout = layout.fruchterman.reingold(dev_graph,dim=3),edge.arrow.width = degree(dev_graph),edge.arrow.size=0.2,vertex.label=NA)

#----------------------- Calculation of Network Measures ----------------------

# Clustering measure - trasitivity of a weighted graph 
dev_transitivity<-transitivity(dev_graph,type="weighted")

# Centrality measures
dev_in_deg<-degree(dev_graph,mode="in",loops=FALSE) # This does not provide weighted degree centrality -- need to fix this
dev_out_deg<-degree(dev_graph,mode="out",loops=FALSE)

dev_cent<-data.frame(dev_in_deg,dev_out_deg)
file_h<-file("/home/srikant/My Documents/Nanohub/Centrality_Measures.csv","w")
write.csv(dev_cent,file_h)
close(file_h)

dev_in_deg_dist<-degree.distribution(dev_graph,mode="in",loops=FALSE,cumulative=FALSE,normalized=FALSE)
dev_out_deg_dist<-degree.distribution(dev_graph,mode="out",loops=FALSE,cumulative=FALSE,normalized=FALSE)
par(mai=c(1,1,1,1))
barplot(dev_in_deg_dist,col=c("blue","red"),xlab="In Degree of Developer",ylab="Frequency")
barplot(dev_out_deg_dist,col=c("blue","red"),xlab="Out Degree of Developer",ylab="Frequency")

ver_bet<-betweenness(dev_graph,directed=TRUE)
edge_bet<-edge.betweenness(dev_graph,directed=TRUE)
# Assortativity calculation 

# needs to be calculated using  sigma ij (di-m)(dj-m)/sigma j (di-m)^2 ij belong to g and i is a vertex 



