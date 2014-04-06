# Author - Srikant Rao

setwd("/home/srikant/My Documents/Nanohub/")

# Load the toolset data 
toolset<-read.csv("dump_NanoHUB_jos_tool_plus_DiD_Mar22.csv")

# Select all the workspace rows 
workspace<-toolset[which(grepl("workspace", toolset$instance)),]

# remove all the rows which have toolname as workspace
toolset<-toolset[- (which(grepl("workspace", toolset$instance)) ),]

# Add the number of Authors 
author_subset<-toolset[,which(grepl("author",colnames(toolset)))]
num_authors<-rowSums(!(author_subset==0)) # Calculate the Number of Authors for each tool
toolset<-data.frame(toolset,num_authors)

rm(author_subset) # remove the temporary subset of author columns that was created.

# Building a tool attribute data set 
# Put this in a separate file -- starting data cleaning is all the code before this 
tool_attrib<-unique(toolset$toolname) # generate list of unique tools -- 831 without workspace 

numtools<-length(tool_attrib)


# 1. User usage statistic
# Add user statisics to each tool in the tool_attrib file 
usage_users<- 1:numtools
num_authors<- 1:numtools
usage_jobs<-1:numtools
latest_rev_pub<-1:numtools
num_cit<-1:numtools
active<-1:numtools
for(i in 1:numtools)
{
  usage_users[i]<- max( toolset$usage_users[which(toolset$toolname == tool_attrib[i])] )
  num_authors[i]<-max( toolset$num_authors[which(toolset$toolname == tool_attrib[i])] )
  usage_jobs[i]<-max( toolset$usage_jobs[which(toolset$toolname == tool_attrib[i])] )
  latest_rev_pub[i]<-max(toolset$revision[which(toolset$toolname == tool_attrib[i])] ) # Capture the maximum revision that has been published
  num_cit[i]<- max(toolset$citations[which(toolset$toolname == tool_attrib[i])] )
  active[i]<-length(which(toolset$state==1 & toolset$toolname ==tool_attrib[i]))
}

# Create the data frame of all attributes captured so far 
tool_attrib<-data.frame(tool_attrib,usage_users,usage_jobs,num_authors,latest_rev_pub,num_cit,active)

# now we need to see who to use the exact correlation between data -- 

# 1. Plot of Simulations vs Users - Should be strongly paired data 
plot(tool_attrib$usage_users,tool_attrib$usage_jobs,log="xy",col="blue",xlab="Number of Users",ylab="Number of Sims")

# 2. Plot of Users vs Num of Authors - Do not know if they are correlated  
plot(tool_attrib$num_authors,tool_attrib$usage_users,log="y",col="blue") # there seems to be no correlation
plot(tool_attrib$num_authors,tool_attrib$usage_jobs,log="y",col="blue") 

# Distribution of Authors for all tools 
hist(tool_attrib$num_authors,col="blue",labels=TRUE,xlab="Number of Authors",ylab="Number of Tools",main="Distribution of Authors")


# 3. Histogram of tool revisions 
# 3.1 Total histogram 
hist(tool_attrib$latest_rev_pub,labels=TRUE,col="blue",xlab="Latest Tool revision",ylab="Number of tools ",main="Distribution of tools with latest revision")
# 3.2 Histogram of tools with latest revision published less than 100 
hist(tool_attrib$latest_rev_pub[tool_attrib$latest_rev_pub <100],labels=TRUE,col="blue",xlab="Latest Tool revision",ylab="Number of tools ",main="Distribution of tools with latest revision <100")
# Usage data 
usage<-read.csv("dump_NanoHUB_tool_usage_plus_DiD.csv")


# Number of Unique users 
num.users<-length(unique(usage$username)) 

# Total number of simulations
num.sim<-length(usage[,1])


# Frequency table of how many times each table has been used 
num.sim.tools<-table(usage$appname) 
# Tool with maximum number of simulations run Leaving aside workspace (215198)
users<-(1:1988)
sims<-(1:1988)
s_wall<-(1:1988)
j_wall<-(1:1988)
## Finding out the uses in each particular tool 
for(i in 1:length(unique.tools))
{
  # Finding the number of Unique users
  users[i]<-length( unique(usage$username[which(usage$appname==unique.tools[i])]))
  # Finding the number of simulations for each tool
  sims[i]<-length(which(usage$appname==unique.tools[i]))
  # Finding the sum of the walltime for each particular tool
  s_wall[i]<-sum( usage$s_walltime[which(usage$appname==unique.tools[i])] )
  j_wall[i]<-sum( usage$j_walltime[which(usage$appname==unique.tools[i])] )
}

# Checks to make sure that the sums are the same 
sum(sims)-length(usage[,1]
sum(s_wall) - sum(usage$s_walltime)   
sum(j_wall) - sum(usage$j_walltime)
# We now have the number of unique users and the number of simulations per tool 


# Combine this into data frame
sim.tools<-data.frame(unique.tools,users,sims)
sim.tools<-data.frame(sim.tools,s_wall,j_wall)
## REMOVE THE value of Workspace from this graph -- It is not an actual tool 
sim.tools<-sim.tools[-4,]

# Plotting these values 
par(mar=c(5, 4, 4, 6) + 0.1)
plot(1:1987, sim.tools$users, pch=16, axes=FALSE, ylim=c(0,4000), xlab="", ylab="", 
     type="b",col="black", main="Simulations and User Data")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Number of Users",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(1:1987, sim.tools$sims, pch=12,  xlab="", ylab="", 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Number of Simulations",side=4,col="red",line=4) 
axis(4, col="red",col.axis="red",las=1)

### ----- Number of tools with specific Number of Users ---------------
length(sim.tools$sims[sim.tools$sims>10]) # 1650
length(sim.tools$sims[sim.tools$sims>100]) # 934
length(sim.tools$sims[sim.tools$sims>1000]) # 355
length(sim.tools$sims[sim.tools$sims>10000]) #60
length(sim.tools$sims[sim.tools$sims>100000]) #1

### -----------------Avg Usage Statistics ---------------------------------
avg_user<-sum(sim.tools$users)/length(sim.tools$users) # 97.32763
avg_sims<-sum(sim.tools$sims)/length(sim.tools$sims) # 1323.952

###- Wall time and CPU time ------------------ 
# work with the walltime subset
plot(1:1987,s_wall,log="y",xlab="Tools",ylab="S Wall time",main="S Wall time for each tool",col="blue")

plot(1:1987,j_wall,log="y",xlab="Tools",ylab="J Wall time",main="J Wall time for each tool",col="blue")

##----------- Country Statistics ---------------------------------------------------------------
country.stat<-table(usage$X_ipCountry)
which(country.stat==max(country.stat))
country.stat<-country.stat[-122] # Delete the US as they have a very large number of simulations performed
max(country.stat)
which(country.stat==max(country.stat))

# Plot the values without India and the US - Outliers 
# US number of Simulations -    1752064
# India number of Simulations - 247603
barplot(country.stat[-56],xlab="Country",ylab="Number of Users",ylim=c(0,70000),main="Country wise Simulation Statistics",col=c("blue","red"))

### --------------------- Adding Author information to the data frame--------------------------------------
auth<-0
for(i in 1:1987)
{
auth[i]<-max(nanohub_toolset$num_authors[which(as.character(nanohub_toolset$toolname) == as.character(sim.tools$sim.tools[i]))])
}
## Open Ended Question ---- what do you about workspace ??
