#--------------------------------------
#DICTIONARY
#--------------------------------------

#Statistics from PUMFs
#Calgary income mean = 49597 (std. = 43451)
#Calgary income mean for owners = 53607 (60449)
#Calgary income mean for renters = 34355 (13576)
#Calgary dwelling value mean = 492090 (263662)
#Calgary tenure
#% mortgage 69%
#%owners 


#----------------------------------
# Import the data
#1. Census data
#2. Hazard map
#3. Distance data
#4. 
#----------------------------------
setwd("D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata")
df <- data.frame(read.csv(file="StudyData.csv", header=T))
df$percown <- df$Owners/df$Dwellings

#library(data.table)
dt <- data.table(df)
dt[,list(wtmean = weighted.mean(Income, Pop)),by=DN]
dt[,list(wtmean = weighted.mean(Dwellval, Pop)),by=DN]
dt[,list(wtmean = weighted.mean(percown, Pop)),by=DN]

dt[,list(wtmean = weighted.mean(Dwellval, Pop)),by=Evacuated]
dt[,list(wtmean = weighted.mean(Income, Pop)),by=Evacuated]
dt[,list(wtmean = weighted.mean(percown, Pop)),by=Evacuated]

#----------------------------------
# Initialization Phase
#----------------------------------
no_agents <- 10000
no_locs <- 200
A_attributes <- 1
L_attributes <- 1

#Create the agents
id <- data.frame(seq(1:no_agents))
a <- data.frame(matrix(runif(A_attributes*no_agents), nrow=no_agents,ncol=A_attributes))
dfA <- cbind(id,a)

#come up with dynamic names and rename data accordingly
s <- seq(1,A_attributes)
name_list <- paste("A",s, sep="")
name_listf <- c("AID", name_list)
names(dfA) <- name_listf

#Create the landscape within which agents reside
id <- data.frame(seq(1:no_locs))
a <- data.frame(matrix(runif(L_attributes*no_locs), nrow=no_locs,ncol=L_attributes))
dfL <- cbind(id,a)

#come up with dynamic names and rename data accordingly
s <- seq(1,L_attributes)
name_listl <- paste("L",s, sep="")
name_listlf <- c("LID", name_listl)
names(dfL) <- name_listlf

#Assign the landscape to the agents (A1 is the LID where the agent is located)
dfA$LID <- sample(min(dfL$LID):max(dfL$LID),no_agents,replace=T)

#---------------------------------------------------------
# Assign the agents to the landscape (multiple steps...)
#---------------------------------------------------------

#get the population count through aggregation
counts <- aggregate(rep(1, length(paste0(dfA$LID))),by=list(dfA$LID), sum)
names(counts) <- c("LID","L1")

#this updates for specific variable using 'match'
matches <- match(counts$LID, dfL$LID)
dfL$L1[matches] <- counts$L1

#get averages of a selection of agent attributes
agg_list <- c("A1")
name_list[-1]
variables <- aggregate(dfA[agg_list], by=list(dfA$LID),FUN=mean)
names(variables)[names(variables) == "Group.1"] <- "LID"
aggregated_data <- variables[c("LID",agg_list)]

#add a prefix so this can be merged back to agents file
colnames(aggregated_data) <- paste("A_", colnames(aggregated_data), sep="")
colnames(aggregated_data)[1] <- "LID"

#merge aggregate data back to locations file
dfL <- merge(dfL,aggregated_data, by="LID")

#---------------------------------------------------------
# Update the agents file
#---------------------------------------------------------

dfA <- merge(dfA,dfL, by="LID")

#-----------------------------------
#Decision Phase
#D1 - buy insurance
#-----------------------------------

dfA$mu1 <- -2 + dfA$A1 + (dfA$A_A1 - dfA$A1)
dfA$p1 <- 1/(1 + exp(-dfA$mu1))
dfA$d1 <- rbinom(no_agents, 1, dfA$p1)

#Update the agent data based on decisions

s1d1 <- dfA[dfA$d1==1,]
s1d1$A1 <- s1d1$A1*0.9
s2d1 <- dfA[dfA$d1==0,]
dfA <- rbind(s1d1,s2d1)

#-----------------------------------
#Decision Phase
#D2 - Change residence
#-----------------------------------


