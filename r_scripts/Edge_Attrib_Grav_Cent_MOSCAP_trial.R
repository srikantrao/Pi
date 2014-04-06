# First attempt at getting the network using vertices 

## Why vertices -- Using an adjacency matrix is going to become computationally comples, since the matris is sparse ( because the average degree is a small value)
## we can expect that the number of entries is going to be pretty small. 
## Hence store vertex information in one data frame
## Store edge information in another data frame - Use graph.dataframe to get the right kind of connections. 

# Author - Srikant Rao 22nd March 2014 

# setting it to the working directory
setwd("/windows/Users/Shrikant Rao/Documents/Nanohub/SVN/SVNmetrics/") 

# load data into dataframe moscap
moscap<-read.csv("moscap_metrics_DiD.csv")

# List of unique tool developers in moscap_names
# Steps to get unique values 1. Get all the unique values 2. Convert them to character 3. Add them to the vertex_attrib files
moscap_names<-as.character( unique (moscap$username) ) 

# Add the vertex names to the vertex_names vector which will be made a column in the vertex_attrib dataframe

Names<-moscap_names
vertex_attrib<-data.frame(Names)

# size of the adjacency matrix for moscap ( Just calculate this as we might need it later on)
moscap_size<-length(moscap_names)

#Create the initial edge attribute data frames ( stores information about all the edges)
FROM<-as.character("Dummy")
TO<-as.character("Dummy")
WEIGHT<-as.numeric(0)
edge_attrib<-data.frame(FROM,TO,WEIGHT)
edge_attrib$FROM<-as.character(edge_attrib$FROM)
edge_attrib$TO<-as.character(edge_attrib$TO)
edge_attrib$WEIGHT<-as.numeric(edge_attrib$WEIGHT)

# Number of revisions in total 
moscap_rev<-length(moscap[,1])   # could even think of using moscap_rev<-max(moscap$rev)


# Creating the contribution <crbn> column  *******Needs to be changed as per Gravitational centrality measure********
crbn<-vector(mode="numeric",length=moscap_rev)
for(i in 1:moscap_rev)
{
  crbn[i]<-max(moscap$add[i],moscap$del[i]) - 0.5*(min(moscap$add[i],moscap$del[i])) + moscap$chrn[i]; # Prof. Matei's delta function  
}

# Add the contribution <crbn> column to the moscap data frame
moscap<-data.frame(moscap,crbn)


# Creating weights using modified gravitational centrality and adding it to the edge_attributes files
# IMPORTANT -- Looping is backwards
temp_contrib<-0 
temp_weight<-0
new_edge_attrib_row <-c("","",0)
for(i in moscap_rev:2)
{
  currval<-i-1 # value that needs to be passed to j
  temp_contrib = moscap$crbn[i]
 for(j in currval:1)
 {
   if(moscap$username[i]==moscap$username[j])
   {temp_contrib = temp_contrib+moscap$crbn[j];}
   
   else
     {
       distance = moscap$rev[j]-moscap$rev[i];
       if ( length(edge_attrib$FROM[ edge_attrib$FROM ==as.character(moscap$username[i]) & edge_attrib$TO==as.character(moscap$username[j])])==0 )
       {  # Create a new edge link between the two 
          new_edge_attrib_row = c(  as.character(moscap$username[i]) , as.character(moscap$username[j]) , as.numeric(temp_contrib/( distance^2 ))  );
          edge_attrib<-rbind(edge_attrib,new_edge_attrib_row);   next;                
        }  
       else {
        temp_weight <- as.numeric(edge_attrib$WEIGHT[edge_attrib$FROM == as.character(moscap$username[i]) & edge_attrib$TO== as.character( moscap$username[j] )  ]) +  temp_contrib/( distance^2 ) ;
        edge_attrib$WEIGHT[edge_attrib$FROM == as.character(moscap$username[i]) & edge_attrib$TO== as.character( moscap$username[j] )  ]<-temp_weight
       }
     }
 }
}

##Deleting the first dummy row 
edge_attrib<-edge_attrib[-1,]
# Plotting the graph using moscap_matrix (Adjacency matrix for this local tool)

