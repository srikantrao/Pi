# Keep adding tool SVN data to the edge_attrib and vertex_attrib files 
# Keep track of files which have been
# 1. MOSCAP
# 2. CRYSTAL viewer
# 3. ABACUS
# 4. PN Junction Diode          pntoy_metrics 
# 5. Nanosphere Optics Lab      nsoptics_metrics
# 6. Quamtum Dot Lab            qdot_metrics
# 7. Drift Diffusion Lab        drsim_metrics
# 8. MOSFET                     mosfet_metrics
# 9. CNT Bands                  cntbands_metrics
# 10. BandStructure Labs        bandstrlab_metrics

# setting it to the working directory
setwd("/windows/Users/Shrikant Rao/Documents/Nanohub/SVN/SVNmetrics/") 
args <- commandArgs(trailingOnly = TRUE) # Command line Arguments
# load data into dataframe crystal
for(k in 1:length(args))
{
tool<-read.csv(args[k])

# List of unique tool developers in tool_names
# Steps to get unique values 1. Get all the unique values 2. Convert them to character 3. Add them to the vertex_attrib files
tool_names<-as.character( unique (tool$username) ) 

# Add the vertex names to the vertex_names vector which will be made a column in the vertex_attrib dataframe
# only for the first particular instance of the graph
if(k ==1)
{Names<-tool_names
 vertex_attrib<-data.frame(Names)
 FROM<-as.character("Dummy")
 TO<-as.character("Dummy")
 WEIGHT<-as.numeric(0)
 edge_attrib<-data.frame(FROM,TO,WEIGHT)
 edge_attrib$FROM<-as.character(edge_attrib$FROM)
 edge_attrib$TO<-as.character(edge_attrib$TO)
 edge_attrib$WEIGHT<-as.numeric(edge_attrib$WEIGHT)
}
else
{
Names<-tool_names

# Getting all the vertices without any duplicate values 
Names<-union(vertex_attrib$Names,Names)
vertex_attrib<-data.frame(Names)
}
# size of the adjacency matrix for tool ( Just calculate this as we might need it later on)
tool_size<-length(tool_names)


# Number of revisions in total 
tool_rev<-length(tool[,1])   # could even think of using tool_rev<-max(tool$rev)


# Creating the contribution <crbn> column  *******Needs to be changed as per Gravitational centrality measure********
crbn<-vector(mode="numeric",length=tool_rev)
for(j in 1:tool_rev)
{
  crbn[j]<-max(tool$add[j],tool$del[j]) - 0.5*(min(tool$add[j],tool$del[j])) + tool$chrn[j]; # Prof. Matei's delta function  
}

# Add the contribution <crbn> column to the tool data frame
tool<-data.frame(tool,crbn)

# Creating weights using modified gravitational centrality and adding it to the edge_attributes files
# IMPORTANT -- Looping is backwards
temp_contrib<-0 
temp_weight<-0
new_edge_attrib_row <-c("","",0)

if(tool_rev>1) # if there is only one tool revision nothing should be done 
{
for(i in tool_rev:2)
{
  currval<-i-1 # value that needs to be passed to j
  temp_contrib = tool$crbn[i]
  for(j in currval:1)
  {
    if(tool$username[i]==tool$username[j])
    {temp_contrib = temp_contrib+tool$crbn[j];}
    
    else
    {
      distance = tool$rev[j]-tool$rev[i];
      if ( length(edge_attrib$FROM[ edge_attrib$FROM ==as.character(tool$username[i]) & edge_attrib$TO==as.character(tool$username[j])])==0 )
      {  # Create a new edge link between the two 
        new_edge_attrib_row = c(  as.character(tool$username[i]) , as.character(tool$username[j]) , as.numeric(temp_contrib/( distance^2 ))  );
        edge_attrib<-rbind(edge_attrib,new_edge_attrib_row);   next;                
      }  
      else {
        temp_weight <- as.numeric(edge_attrib$WEIGHT[edge_attrib$FROM == as.character(tool$username[i]) & edge_attrib$TO== as.character( tool$username[j] )  ]) +  (temp_contrib*tool$crbn[j])/( distance^2 ) ;
        edge_attrib$WEIGHT[edge_attrib$FROM == as.character(tool$username[i]) & edge_attrib$TO== as.character( tool$username[j] )  ]<-temp_weight
      }
    }
  }
}

}  # End of if (tool_rev>1)
else {next;}
} # End of original file for loop
print(edge_attrib) # List of all the edge attributes that have been formed
