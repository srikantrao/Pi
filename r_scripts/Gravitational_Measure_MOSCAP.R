# First attempt at getting the adjacency matrix for the tool MOSCAP

# Author - Srikant Rao 20th March 2014 

# Current Todo 1. Look at edge based graph, plotting a sprarse matrix is faster using edges instead of adj matrix 
# Current Todo 2. Zooming into graph -- analyze micro characteristics of nodes of interest using igraph ?? 

# setting it to the working directory
setwd("/windows/Users/Shrikant Rao/Documents/Nanohub/SVN/SVNmetrics/") 

# load data into dataframe moscap
moscap<-read.csv("moscap_metrics_DiD.csv")

# List of unique tool developers in moscap_names
moscap_names<-unique(unlist(moscap$username))

# size of the adjacency matrix for moscap
moscap_size<-length(moscap_names)

#Create empty adjacency matrix for tooldevelopers in moscap
# Important to remember - dimnames need to be lists -- hence use list for forming a list of the tool developers
moscap_matrix<-matrix(0,nrow=moscap_size,ncol=moscap_size,dimnames=c(list(moscap_names),list(moscap_names)))

# Number of revisions in total 
moscap_rev<-length(moscap[,1])   # could even think of using moscap_rev<-max(moscap$rev)


# Creating the contribution <crbn> column  *******Needs to be changed as per Gravitational centrality measure********
crbn<-vector(mode="numeric",length=moscap_rev)
for(i in 1:moscap_rev)
{
  crbn[i]<-max(moscap$add[i],moscap$del[i]) - 0.5*(min(moscap$add[i],moscap$del[i])) + moscap$chrn[i]; # Prof. Matei's delta function  
}
moscap_length<-1:moscap_rev
# Add the contribution <crbn> column to the moscap data frame
moscap<-data.frame(moscap,crbn)
temp_contrib<-0 
# Creating weights using modified gravitational centrality 
# IMPORTANT -- Looping is backwards
for(i in moscap_rev:2)
{currval<-i-1 # value that needs to be passed to j
 temp_contrib = moscap$crbn[i]
  for(j in currval:1)
  {
   if(moscap$username[i]==moscap$username[j])
     {temp_contrib = temp_contrib+moscap$crbn[j];next;}
   else{distance = moscap$rev[j]-moscap$rev[i];
        moscap_matrix[moscap$username[i],moscap$username[j]]=temp_contrib/(distance^2);
   }
  }
}

# Plotting the graph using moscap_matrix (Adjacency matrix for this local tool)

library("igraph") # Look at statnet for additional features 
# Graphing using adjacency matrix
moscap_graph<-graph.adjacency(moscap_matrix,mode="directed",weighted=TRUE)

#plotting the graph 

# Plotting parameters 
V(moscap_graph)$size = degree(moscap_graph)
V(moscap_graph)$color = c("red","grey","blue","yellow","cyan","purple","green","brown","violet")
par(mai=c(0.5,0.5,1,0.5))   		#Specify size of margins
plot(moscap_graph,			#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='MOSCAP Tool Author Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=100,			#the font of the name labels
     vertex.label=V(moscap_graph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.25,		#specifies the size of the font of the labels. can also be made to vary                               # Reduce the size of the nodes
     edge.arrow.size = 0.2,
     edge.width = E(moscap_graph)*0.1 # set the 
     )

# 3D plot -- currently useless .. need to find 3D layout and add all the nodes for better viewing 
rglplot(moscap_graph,layout=layout.fruchterman.reingold,edge.arrow.width = degree(moscap_graph),edge.arrow.size=0.2,vertex.label.font=8)

#----------------------- Calculation of Network Measures ----------------------

# Clustering measure - trasitivity of a weighted graph 
moscap_transitivity<-transitivity(moscap_graph,type="weighted")

# Centrality measures
moscap_in_deg<-degree(moscap_graph,mode="in",loops=FALSE) # This does not provide weighted degree centrality -- need to fix this
moscap_out_deg<-degree(moscap_graph,mode="out",loops=FALSE)
moscap_in_deg_dist<-degree.distribution(moscap_graph,mode="in",loops=FALSE,cumulative=FALSE,normalized=FALSE)
moscap_out_deg_dist<-degree.distribution(moscap_graph,mode="out",loops=FALSE,cumulative=FALSE,normalized=FALSE)
plot(moscap_in_deg_dist,xlim=c(0,10),ylim=c(0.0,0.3),col="blue",xlab="In Degree of Author",ylab="Frequency")
plot(moscap_out_deg_dist,xlim=c(0,10),ylim=c(0.0,0.3),col="blue",xlab="Out Degree of Author",ylab="Frequency")

# Assortativity calculation 

# needs to be calculated using  sigma ij (di-m)(dj-m)/sigma j (di-m)^2 ij belong to g and i is a vertex 


#-------------- Adding another data file to the network -----------------------------------
