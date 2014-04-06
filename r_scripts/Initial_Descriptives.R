# Script for initial descriptive analysis of Tool Dataset

# Author - Srikant Rao

setwd("C:/Users/Shrikant Rao/Documents/Nanohub/R Scripts") 

#---------------------------------------------------------------------------

# Accessing the active set - Only those with NA unpublished values will be used

nanohub_toolset<-read.csv("nanohub_toolset.csv")

#------------------ Loading the Active Dataset using state as the key-------------------------
array<-which(is.na(nanohub_toolset$unpublished_ymd)) # All the tools which currently have an unpublished_ymd
active_toolset<-nanohub_toolset[array,]

# active_toolset<-nanohub_toolset[which(is.na(nanohub_toolset$unpublished_ymd)),] 

# active_toolset contains all the tools which are currently up on the NanoHub Site

#------------------------------------------------------------------------------

# Revision Information of currently active toolset

mean(as.integer(active_toolset$revision)) # Revision descriptives
sd(as.integer(active_toolset$revision))
max(as.integer(active_toolset$revision))
min(as.integer(active_toolset$revision))

active_revisions<-as.integer(active_toolset$revision)
hist(active_revisions,col="blue",border="black",main="Tool Revision Distribution",breaks=35,ylab="Number of Tools",xlab="Number of Revision",label=TRUE)

#------------------------Duration Information in days-------------------------------------------------------
Mar_16_2014<-731688 # Today's date according to the number of days calculator in NanoHub
duration<-Mar_16_2014 - as.integer(active_toolset$released_ymd) 

mean(duration, na.rm=TRUE) # Do not include missing values in computation
max(duration,na.rm=TRUE)
min(duration,na.rm=TRUE)

hist(duration,col="blue",border="black",main="Tool Duration Distribution",breaks=35,ylab="Number of Tools",xlab="Duration",label=TRUE)

# Plot Duration vs Number of Revisions
plot(duration,active_revisions,xlab="Online Duration of Tool (days)",ylab="Number of Revision",main="Revision vs Duration",col="blue")

# ------------------------------------------------------------------------
# Code Access, Wiki Access and Tool Access with bar plots 

#Categorical variable of Code Access
codeAccess<-table(active_toolset $ codeaccess)
barplot(codeAccess,ylab="Number of Tools",xlab="Code Access",names.arg=c("CLOSED","OPEN","NA"))

# Categorical variable of Wiki Access
wikiAccess<-table(active_toolset $ wikiaccess)
barplot(wikiAccess,ylab="Number of Tools",xlab="Wiki Access",names.arg=c("CLOSED","OPEN","NA"),col="blue",border="black")

# Categorical variable of Tool Access
toolAccess<-table(active_toolset $ toolaccess)
barplot(toolAccess,ylab="Number of Tools",xlab="Tool Access",cex.lab=1.25,tck=1,names.arg=c("D1","GROUP","OPEN","PU","US","NA"),col="blue",border="black")

#--------------------------------------------------------------------------

# Author Information 

# Getting the Author subset from the active_toolset dataframe 
author_subset<-nanohub_toolset[,which(grepl("author",colnames(nanohub_toolset)))]
num_authors<-rowSums(!(author_subset==0)) # Calculate the Number of Authors for each tool
nanohub_toolset<-data.frame(nanohub_toolset,num_authors)

hist(num_authors,col="blue",border="black",main="Distribution of Number of Authors",ylab="Number of Tools",xlab="Number of Authors",label=TRUE)
active_to
