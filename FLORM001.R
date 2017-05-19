#--------------------------------------
#DICTIONARY
#--------------------------------------

#Statistics from PUMFs
#Calgary income mean = mean (std. = mean)
#Calgary income mean for owners = 53607 (60449)
#Calgary income mean for renters = 34355 (13576)
#Calgary tenure
#% mortgage 69%
#%owners 

#----------------------------------
# Import the data
#----------------------------------

setwd("D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata")
df <- data.frame(read.csv(file="StudyData.csv", header=T))
df2 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\StatsData\\EducationData.csv", header=T))
df <- merge(df,df2,by="DAUID")

df$percown <- df$Owners/df$Dwellings

#library(data.table)
#library(truncnorm)
dt <- data.table(df)
dt[,list(wtmean = weighted.mean(Income, Pop)),by=DN]
dt[,list(wtmean = weighted.mean(Dwellval, Pop)),by=DN]
dt[,list(wtmean = weighted.mean(percown, Pop)),by=DN]

dt[,list(wtmean = weighted.mean(Dwellval, Pop)),by=Evacuated]
dt[,list(wtmean = weighted.mean(Income, Pop)),by=Evacuated]
dt[,list(wtmean = weighted.mean(percown, Pop)),by=Evacuated]

#----------------------------------
# Models
#----------------------------------

df$vlrisk[df$DN == 1] <- 0
df$vlrisk[df$DN > 1] <- 1

df$E[df$Evacuated == "Yes"] <-1
df$E[df$Evacuated == "No"] <-0

#factors that may raise purchase overall include
#1. Trust in insurers / insurer credibility
#2. Regulations that force mortgage holders to seek insurance (SFHA)
#3. History of government assistance
#4. History/frequency of flood events

#Based on Atreya et al.
#dependent variable is log(policies per 1000 population)
#prediction does not account for the ecological variables, and a few other strange little things wrt dummy variables
sum_buyers <- 0
running_price <- 0
for(j in seq(from=1, to=4, by=1))
{
price <- j # this is price per $1000 in coverage
time <- 1
df$WTBA <- 0
  for(i in 1:100)
  {
    df$ln_WTB <- -7.172 + 
    0.390*log(df$Income) + 
    -0.156*log(price) +
    0.026*log(time) + #has to be hardcoded
    0.018*log(rtruncnorm(1545,a=0,b=3.366,mean=0.004,sd=0.083)) + #mitigation random effect
    1.009*df$vlrisk + #proxy for % FloodPlain
    -0.0327*rtruncnorm(1545,a=0,b=31.36,mean=1.08,sd=3.28) + #NoNFIP
    0.0105*rtruncnorm(1545,a=0,b=82.88,mean=25.6,sd=17.17)  + #%AA
    -0.0002*rtruncnorm(1545,a=0,b=82.88,mean=25.6,sd=17.17)  + #White
    0.0435*rtruncnorm(1545,a=18.9,b=98.8,mean=69,sd=16.52)  + #high school
    0.044*df$percedu + #college/university
    -0.0005*df$percown + #own
    0.0043*rtruncnorm(1545,a=16.9, b=66.96,mean=32.2,sd=6.39) + #age25-44
    0.0472*rtruncnorm(1545,a=1.32, b=34.7, mean=21.12,sd=4.36) + #age45-65
    0.0565*rtruncnorm(1545,a=0.43, b=29.2,mean=11.08,sd=3.54) #age 65+
    df$WTB01 <- exp(df$ln_WTB)
    df$WTBA <- df$WTB01 + df$WTBA
  }
df$WTBA <- df$WTBA/i
df$buyers <- (df$WTBA/1000) * df$Dwellings
sum_buyers <- rbind(sum_buyers,sum(df$buyers))
running_price <- rbind(running_price,price)
}
elasticity <- cbind(running_price,sum_buyers)

output <- df[c("DAUID","WTBA")]
write.csv(output,file="model01.csv")

#-------------------------------------------------
#
#-------------------------------------------------

#Based on Hung et al., 2009
price <- 250
df$WTB02 <- -4.560 +
  0.012*(df$Income/31000) + #exchange rate and per 1000
  -.384*price/31 + #exchange rate
  1.25*df$E + #equivalent of being affected by flood
  1.18*df$percown +
  0.89*df$percedu +
  rnorm(0,0.5)
df$WTB02 <- exp(df$WTB02)
df$WTB02 <- df$WTB02 / (1 + df$WTB02) #as a probability




#----------------------------------
# Initialization Phase
#----------------------------------



#----------------------------------
# Initialization Phase
#----------------------------------
no_agents <- sum(df$Dwellings)
no_locs <- length(df$Dwellings)
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


