# Adding Crystal Viewer Edge list to the already created graph 
## Very important to do - FIND WAY OF SCRIPTING FILE INPUT ( Only thing remaining if this works)

## Why vertices -- Using an adjacency matrix is going to become computationally comples, since the matris is sparse ( because the average degree is a small value)
## we can expect that the number of entries is going to be pretty small. 
## Hence store vertex information in one data frame
## Store edge information in another data frame - Use graph.dataframe to get the right kind of connections. 

# Author - Srikant Rao 23rd March 2014 

# setting it to the working directory
setwd("/windows/Users/Shrikant Rao/Documents/Nanohub/SVN/SVNmetrics/") 

# load data into dataframe crystal
crystal<-read.csv("crystal_viewer_metrics_DiD.csv")

# List of unique tool developers in crystal_names
# Steps to get unique values 1. Get all the unique values 2. Convert them to character 3. Add them to the vertex_attrib files
crystal_names<-as.character( unique (crystal$username) ) 

# Add the vertex names to the vertex_names vector which will be made a column in the vertex_attrib dataframe

Names<-crystal_names
 
# Getting all the vertices without any duplicate values 
Names<-union(vertex_attrib$Names,Names)
vertex_attrib<-data.frame(Names)

# size of the adjacency matrix for crystal ( Just calculate this as we might need it later on)
crystal_size<-length(crystal_names)


# Number of revisions in total 
crystal_rev<-length(crystal[,1])   # could even think of using crystal_rev<-max(crystal$rev)


# Creating the contribution <crbn> column  *******Needs to be changed as per Gravitational centrality measure********
crbn<-vector(mode="numeric",length=crystal_rev)
for(i in 1:crystal_rev)
{
  crbn[i]<-max(crystal$add[i],crystal$del[i]) - 0.5*(min(crystal$add[i],crystal$del[i])) + crystal$chrn[i]; # Prof. Matei's delta function  
}

# Add the contribution <crbn> column to the crystal data frame
crystal<-data.frame(crystal,crbn)

# Creating weights using modified gravitational centrality and adding it to the edge_attributes files
# IMPORTANT -- Looping is backwards
temp_contrib<-0 
temp_weight<-0
new_edge_attrib_row <-c("","",0)
for(i in crystal_rev:2)
{
  currval<-i-1 # value that needs to be passed to j
  temp_contrib = crystal$crbn[i]
  for(j in currval:1)
  {
    if(crystal$username[i]==crystal$username[j])
    {temp_contrib = temp_contrib+crystal$crbn[j];}
    
    else
    {
      distance = crystal$rev[j]-crystal$rev[i];
      if ( length(edge_attrib$FROM[ edge_attrib$FROM ==as.character(crystal$username[i]) & edge_attrib$TO==as.character(crystal$username[j])])==0 )
      {  # Create a new edge link between the two 
        new_edge_attrib_row = c(  as.character(crystal$username[i]) , as.character(crystal$username[j]) , as.numeric(temp_contrib/( distance^2 ))  );
        edge_attrib<-rbind(edge_attrib,new_edge_attrib_row);   next;                
      }  
      else {
        temp_weight <- as.numeric(edge_attrib$WEIGHT[edge_attrib$FROM == as.character(crystal$username[i]) & edge_attrib$TO== as.character( crystal$username[j] )  ]) +  (temp_contrib*tool$crbn[j])/( distance^2 ) ;
        edge_attrib$WEIGHT[edge_attrib$FROM == as.character(crystal$username[i]) & edge_attrib$TO== as.character( crystal$username[j] )  ]<-temp_weight
      }
    }
  }
}

# Plotting the graph using crystal_matrix (Adjacency matrix for this local tool)

library("igraph") # Look at statnet for additional features 

author_graph <- graph.data.frame(edge_attrib, directed=TRUE, vertices=vertex_attrib)
#plotting the graph 

# Plotting parameters 
V(author_graph)$size = degree(author_graph)*0.3
V(author_graph)$color = c("red","grey","blue","yellow","cyan","purple","green","brown","violet")
par(mai=c(0,0,1,0))       #Specify size of margins
plot(author_graph,			#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Two Tools Author Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=100,			#the font of the name labels
     vertex.label=V(author_graph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.25,		#specifies the size of the font of the labels. can also be made to vary                               # Reduce the size of the nodes
     edge.arrow.size = 0.2,
     edge.width = E(author_graph)*0.005 # set the 
)

# 3D plot -- currently useless .. need to find 3D layout and add all the nodes for better viewing 
rglplot(author_graph,layout=layout.fruchterman.reingold,edge.arrow.width = degree(author_graph),edge.arrow.size=0.2,vertex.label.font=8)

#----------------------- Calculation of Network Measures ----------------------

# Clustering measure - trasitivity of a weighted graph 
crystal_transitivity<-transitivity(author_graph,type="weighted")

# Centrality measures
crystal_in_deg<-degree(author_graph,mode="in",loops=FALSE) # This does not provide weighted degree centrality -- need to fix this
crystal_out_deg<-degree(author_graph,mode="out",loops=FALSE)
crystal_in_deg_dist<-degree.distribution(author_graph,mode="in",loops=FALSE,cumulative=FALSE,normalized=FALSE)
crystal_out_deg_dist<-degree.distribution(author_graph,mode="out",loops=FALSE,cumulative=FALSE,normalized=FALSE)
# plot(crystal_in_deg_dist,xlim=c(0,10),ylim=c(0.0,0.3),col="blue",xlab="In Degree of Author",ylab="Frequency")
# plot(crystal_out_deg_dist,xlim=c(0,10),ylim=c(0.0,0.3),col="blue",xlab="Out Degree of Author",ylab="Frequency")

# Assortativity calculation 

# needs to be calculated using  sigma ij (di-m)(dj-m)/sigma j (di-m)^2 ij belong to g and i is a vertex 


#-------------- Adding another data file to the network -----------------------------------
