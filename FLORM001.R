#--------------------------------------
#DICTIONARY
#--------------------------------------

#library(ggplot2)
#library(data.table)
#library(truncnorm)

#Statistics from PUMFs
#Calgary income mean = mean (std. = mean)
#Calgary income mean for owners = 53607 (60449)
#Calgary income mean for renters = 34355 (13576)
#Calgary tenure
#% mortgage 69%
#%owners 

#----------------------------------
# Set global indicators
#----------------------------------

ITERATIONS <- 10

#----------------------------------
# Import the data
#----------------------------------

setwd("D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata")
df <- data.frame(read.csv(file="StudyData.csv", header=T))
df2 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\StatsData\\EducationData.csv", header=T))
df3 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\DAUID_distance_buffer.csv", header=T))
df4 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\Distance_to_water_m_DAUID.csv", header=T))
df5 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\Calgary_elevation_DAs.csv", header=T))
df6 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\DAUID_65plus.csv", header=T))
df7 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\age_categories.csv", header=T))
df8 <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\GISdata\\education2.csv", header=T))

min <- min(df5$Elevation)
df5$Elevation <- df5$Elevation - min #approximate elevation above river

df <- merge(df,df2,by="DAUID")
df <- merge(df,df3,by="DAUID")
df <- merge(df,df4,by="DAUID")
df <- merge(df,df5,by="DAUID")
df <- merge(df,df6,by="DAUID")
df <- merge(df,df7,by="DAUID")
df <- merge(df,df8,by="DAUID")
df$percown <- df$Owners/df$Dwellings
df$Dwellval[df$Dwellval==0] <- 500000

df$incomecat[df$Income < 5000] <- 1
df$incomecat[df$Income >= 5000 & df$Income < 20000] <- 2
df$incomecat[df$Income >= 20000 & df$Income < 30000] <- 3
df$incomecat[df$Income >= 30000 & df$Income < 40000] <- 4
df$incomecat[df$Income >= 40000 & df$Income < 50000] <- 5
df$incomecat[df$Income >= 50000 & df$Income < 60000] <- 6
df$incomecat[df$Income >= 60000 & df$Income < 70000] <- 7
df$incomecat[df$Income >= 70000 & df$Income < 80000] <- 8
df$incomecat[df$Income >= 80000 & df$Income < 90000] <- 9
df$incomecat[df$Income >= 90000 & df$Income < 100000] <- 10
df$incomecat[df$Income >= 100000 & df$Income < 110000] <- 11
df$incomecat[df$Income >= 110000 & df$Income < 120000] <- 12
df$incomecat[df$Income >= 120000 & df$Income < 130000] <- 13
df$incomecat[df$Income >= 130000 & df$Income < 140000] <- 14
df$incomecat[df$Income >= 140000 & df$Income < 150000] <- 15
df$incomecat[df$Income >= 150000 & df$Income < 160000] <- 16
df$incomecat[df$Income >= 160000 & df$Income < 170000] <- 17
df$incomecat[df$Income >= 170000] <- 18

df$vlrisk[df$DN == 1] <- 0
df$vlrisk[df$DN > 1] <- 1

df$E[df$Evacuated == "Yes"] <-1
df$E[df$Evacuated == "No"] <-0

df$percown <- df$percown*100
df$percedu <- df$percedu*100

write.csv(df,file="D:\\ALLWORK\\PROJECTS\\Floodnet\\StatsData\\WorkingFile.csv")

#----------------------------------
# Descriptives
#----------------------------------

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

#factors that may raise purchase overall include
#1. Trust in insurers / insurer credibility
#2. Regulations that force mortgage holders to seek insurance (SFHA)
#3. History of government assistance
#4. History/frequency of flood events

#-----------------------------------------
#MODEL 1 Based on Atreya et al.
#ECOLOGICAL MODEL (counties)
#-----------------------------------------

#dependent variable is log(policies per 1000 population)
#prediction does not account for the ecological variables, and a few other strange little things wrt dummy variables
sum_buyers <- 0
sum_upper <- 0
sum_lower <- 0
running_price <- 0
sd_matrix <- matrix(0,nrow=1543,ncol=ITERATIONS)
price_matrix <- matrix(0,nrow=1543,ncol=40)
for(j in seq(from=1, to=10, by=1))
{
  price <- j # this is price per $1000 in coverage
  time <- 1
  df$WTBA <- 0
  for(i in 1:ITERATIONS)
  {
    df$AA <- rtruncnorm(1543,a=0,b=82.88,mean=25.6,sd=17.17)
    df$W <- 100-df$AA
    df$ln_WTB <- -7.172 + 
      0.390*log(df$Income) + 
      -0.156*log(price) +
      0.026*log(time) + #has to be hardcoded
      0.018*llog(rtruncnorm(1543,a=0,b=3.366,mean=0.004,sd=0.083)) + #mitigation random effect
      1.009*df$vlrisk + #proxy for % FloodPlain
      -0.0327*rtruncnorm(1543,a=0,b=31.36,mean=1.08,sd=3.28) + #NoNFIP
      0.0105*df$AA + #%AA
      -0.0002*df$W  + #White
      0.0435*df$phigh + #high school
      0.044*df$percedu + #college/university
      -0.0005*df$percown + #own
      0.001*(100-df$percown) + #rent
      0.0043*df$age2544 + #age25-44
      0.0472*df$age4564 + #age45-65
      0.0565*df$age65. #age 65+
    df$WTB01 <- exp(df$ln_WTB)
    df$WTBA <- df$WTB01 + df$WTBA
    sd_matrix[,i] <- df$WTB01/1000 #store the value in a matrix
  }
  price_matrix[,j] <- apply(sd_matrix,1,mean) #take the mean of prediction for price j
  price_matrix[,j+10] <- apply(sd_matrix,1,sd) #take the sd of prediction for price j
  df$buyers <- price_matrix[,j]*df$Dwellings #calculate the number of dwellings that would buy at price j
  df$upper_buyers <- price_matrix[,j]*df$Dwellings + price_matrix[,j+10]*df$Dwellings
  df$lower_buyers <- price_matrix[,j]*df$Dwellings - price_matrix[,j+10]*df$Dwellings
  sum_upper <- rbind(sum_upper,sum(df$upper_buyers))
  sum_lower <- rbind(sum_lower,sum(df$lower_buyers))
  sum_buyers <- rbind(sum_buyers,sum(df$buyers))
  running_price <- rbind(running_price,price)
}

pricevurve01 <- cbind(running_price,sum_buyers,sum_upper,sum_lower)
pricevurve01 <- as.data.frame(pricevurve01)
pricevurve01 <- pricevurve01[!(pricevurve01$V1==0),]
pricevurve01$m <- "Atreya"


households_by_price01 <- price_matrix[,1:10]*df$Dwellings

output <- df[c("DAUID")]
output <- cbind(output,households_by_price01)

write.csv(output,file="model01.csv")

#-------------------------------------------------
# MODEL 2 - Based on Browne add Hoyt 2000
#ECOLOGICAL MODEL (state)
#-------------------------------------------------

#premium per 1000 have to adjust for inflation here (+ 27%)
sum_buyers <- 0
sum_upper <- 0
sum_lower <- 0
running_price <- 0
sd_matrix <- matrix(0,nrow=1543,ncol=ITERATIONS)
price_matrix <- matrix(0,nrow=1543,ncol=20)
for(j in seq(from=1, to=10, by=1))
{
  price <- j + (j*.27) # this is price per $1000 in coverage
  df$WTBA <- 0
  for(i in 1:ITERATIONS)
  {
    df$ln_WTB <- 0.807 + 
      -0.007*rtruncnorm(1543,a=0,b=1.6,mean=0.463,sd=0.533) + #mitigation expenses per capita
      0.009*rtruncnorm(1543,a=0,b=9.5,mean=1.815,sd=3.162) + #disaster relief
      -0.109*log(price) + #price of the insurance
      1.4*log(((df$Income/1000)+((df$Income/1000)*0.27)/6.5)) + #have to adjust for inflation and disposable income vs. income (Canada)
      -0.056*rtruncnorm(1543,a=0,b=3.9,mean=2.817,sd=1.316) +
      0.017*rtruncnorm(1543,a=0,b=108.2,mean=9.825,sd=36.052)
    df$WTB02 <- df$ln_WTB/1000 #i did not exponentiate this because it looks like the authors made an error
    df$WTBA <- df$WTB02 + df$WTBA
    sd_matrix[,i] <- df$WTB02
  }
  price_matrix[,j] <- apply(sd_matrix,1,mean) #take the mean of prediction for price j
  price_matrix[,j+10] <- apply(sd_matrix,1,sd) #take the sd of prediction for price j
  df$buyers <- price_matrix[,j]*df$Dwellings #calculate the number of dwellings that would buy at price j
  df$upper_buyers <- price_matrix[,j]*df$Dwellings + price_matrix[,j+10]*df$Dwellings
  df$lower_buyers <- price_matrix[,j]*df$Dwellings - price_matrix[,j+10]*df$Dwellings
  sum_upper <- rbind(sum_upper,sum(df$upper_buyers))
  sum_lower <- rbind(sum_lower,sum(df$lower_buyers))
  sum_buyers <- rbind(sum_buyers,sum(df$buyers))
  running_price <- rbind(running_price,j)
}
pricevurve02 <- cbind(running_price,sum_buyers,sum_upper,sum_lower)
pricevurve02 <- as.data.frame(pricevurve02)
pricevurve02 <- pricevurve02[!(pricevurve02$V1==0),]
pricevurve02$m <- "Browne"


households_by_price02 <- price_matrix[,1:10]*df$Dwellings

output <- df[c("DAUID")]
output <- cbind(output,households_by_price02)

write.csv(output,file="model02.csv")

#-------------------------------------------------
# MODEL 3 - Based on Hung et al., 2009
#-------------------------------------------------

sum_buyers <- 0
sum_upper <- 0
sum_lower <- 0
running_price <- 0
sd_matrix <- matrix(0,nrow=1543,ncol=ITERATIONS)
for(j in seq(from=1, to=10, by=1))
{
  price <- j
  df$WTBA <- 0
  for(i in 1:ITERATIONS)
  {
    df$WTB03 <- -4.560 +
      3.190*rtruncnorm(1543,a=0,b=1,mean=0.48,sd=0.5) + #agree with government policy
      -0.384*(price*1000)/22 + #price accounting for exchange rate
      0.430*df$In200m + #distance 200
      0.920*df$In400m + #distance 400
      -0.560*rtruncnorm(1543,a=0,b=1,mean=0.63,sd=0.48) + #sex
      0.017*df$med_age + #Age
      0.894*df$percedu/100 + #Education (a proportion)
      0.012*(df$Income*22)/1000 + #exchange rate and per 1000
      1.25*df$E + #equivalent of being affected by flood
      1.18*df$percown + #%own
      0.136*rtruncnorm(1543,a=0,b=12.21,mean=13.09,sd=4.07) + #years in house
      0.602*rtruncnorm(1543,a=0,b=3,mean=.74,sd=.75)  +  #pro insurnace
      -0.550*rtruncnorm(1543,a=1,5,mean=3.03,sd=0.77)  + #govt inform
      0.606*rtruncnorm(1543,a=1,b=5,mean=3.36,sd=0.75)  + #elect inform
      -1.438*rtruncnorm(1543,a=0,1,mean=0.81,sd=0.39)  + #perceived risk
      -0.812*rtruncnorm(1543,a=1,b=5,mean=2.28,sd=0.71)  # mitigation
    df$WTB03 <- exp(df$WTB03)
    df$WTB03 <- df$WTB03 / (1 + df$WTB03) #as a probability
    sd_matrix[,i] <- df$WTB03 #store the value in a matrix
    df$WTBA <- df$WTB03 + df$WTBA
  }
  price_matrix[,j] <- apply(sd_matrix,1,mean) #take the mean of prediction for price j
  price_matrix[,j+10] <- apply(sd_matrix,1,sd) #take the sd of prediction for price j
  df$buyers <- price_matrix[,j]*df$Dwellings #calculate the number of dwellings that would buy at price j
  df$upper_buyers <- price_matrix[,j]*df$Dwellings + price_matrix[,j+10]*df$Dwellings
  df$lower_buyers <- price_matrix[,j]*df$Dwellings - price_matrix[,j+10]*df$Dwellings
  sum_upper <- rbind(sum_upper,sum(df$upper_buyers))
  sum_lower <- rbind(sum_lower,sum(df$lower_buyers))
  sum_buyers <- rbind(sum_buyers,sum(df$buyers))
  running_price <- rbind(running_price,price)
}
pricevurve03 <- cbind(running_price,sum_buyers,sum_upper,sum_lower)
pricevurve03 <- as.data.frame(pricevurve03)
pricevurve03 <- pricevurve03[!(pricevurve03$V1==0),]
pricevurve03$m <- "Hung"


households_by_price03 <- price_matrix[,1:10]*df$Dwellings

output <- df[c("DAUID")]
output <- cbind(output,households_by_price03)

write.csv(output,file="model03.csv")

#-------------------------------------------------
# MODEL 4 - Based on Kriesel and Landry 2004
#-------------------------------------------------

sum_buyers <- 0
sum_upper <- 0
sum_lower <- 0
running_price <- 0
sd_matrix <- matrix(0,nrow=1543,ncol=ITERATIONS)
for(j in seq(from=1, to=10, by=1))
{
  price <- j
  df$WTBA <- 0
  for(i in 1:ITERATIONS)
  {
    df$WTB04 <- -0.111 +
      -0.591*price/10 + #because paper looks at price per $100
      0.0048*df$Income/1000 + #exchange rate and per 1000 
      -0.0023*df$Distance_m*.62137 + #distance, probably in miles...will check
      0.491*0 + #artificial protection - leave as 0 since it doesn't exist...
      4.359*0#rtruncnorm(1543,a=0,b=2,mean=0.275,sd=0.58) + #mortgage
    -0.010*3 #since hurricane -- treat as years since floods?
    df$WTB04 <- exp(df$WTB04)
    df$WTB04 <- df$WTB04 / (1 + df$WTB04) #as a probability logit
    sd_matrix[,i] <- df$WTB04 #store the value in a matrix
    df$WTBA <- df$WTB04 + df$WTBA
  }
  price_matrix[,j] <- apply(sd_matrix,1,mean) #take the mean of prediction for price j
  price_matrix[,j+10] <- apply(sd_matrix,1,sd) #take the sd of prediction for price j
  df$buyers <- price_matrix[,j]*df$Dwellings #calculate the number of dwellings that would buy at price j
  df$upper_buyers <- price_matrix[,j]*df$Dwellings + price_matrix[,j+10]*df$Dwellings
  df$lower_buyers <- price_matrix[,j]*df$Dwellings - price_matrix[,j+10]*df$Dwellings
  sum_upper <- rbind(sum_upper,sum(df$upper_buyers))
  sum_lower <- rbind(sum_lower,sum(df$lower_buyers))
  sum_buyers <- rbind(sum_buyers,sum(df$buyers))
  running_price <- rbind(running_price,price)
}
pricevurve04 <- cbind(running_price,sum_buyers,sum_upper,sum_lower)
pricevurve04 <- as.data.frame(pricevurve04)
pricevurve04 <- pricevurve04[!(pricevurve04$V1==0),]
pricevurve04$m <- "Kriesel"

households_by_price04 <- price_matrix[,1:10]*df$Dwellings

output <- df[c("DAUID")]
output <- cbind(output,households_by_price04)

write.csv(output,file="model04.csv")


#-------------------------------------------------
# MODEL 5 - Shao et al., 2017
#-------------------------------------------------

sum_buyers <- 0
sum_upper <- 0
sum_lower <- 0
running_price <- 0
sd_matrix <- matrix(0,nrow=1543,ncol=ITERATIONS)
price_matrix <- matrix(0,nrow=1543,ncol=40)
for(j in seq(from=1, to=10, by=1))
{
  price <- j # this is price per $1000 in coverage
  time <- 1
  df$WTBA <- 0
  for(i in 1:ITERATIONS)
  {
    df$AA <- rtruncnorm(1543,a=0,b=82.88,mean=25.6,sd=17.17)
    df$W <- 100-df$AA
    df$ln_WTB <- -7.172 + 
      0.390*log(df$Income) + 
      -0.156*log(price) +
      0.026*log(time) + #has to be hardcoded
      0.018*log(rtruncnorm(1543,a=0,b=3.366,mean=0.004,sd=0.083)) + #mitigation random effect
      1.009*df$vlrisk + #proxy for % FloodPlain
      -0.0327*rtruncnorm(1543,a=0,b=31.36,mean=1.08,sd=3.28) + #NoNFIP
      0.0105*df$AA + #%AA
      -0.0002*df$W  + #White
      0.0435*df$phigh + #high school
      0.044*df$percedu + #college/university
      -0.0005*df$percown + #own
      0.001*(100-df$percown) + #rent
      0.0043*df$age2544 + #age25-44
      0.0472*df$age4564 + #age45-65
      0.0565*df$age65. #age 65+
    df$WTB01 <- exp(df$ln_WTB)
    df$WTBA <- df$WTB01 + df$WTBA
    sd_matrix[,i] <- df$WTB01/1000 #store the value in a matrix
  }
  price_matrix[,j] <- apply(sd_matrix,1,mean) #take the mean of prediction for price j
  price_matrix[,j+10] <- apply(sd_matrix,1,sd) #take the sd of prediction for price j
  df$buyers <- price_matrix[,j]*df$Dwellings #calculate the number of dwellings that would buy at price j
  df$upper_buyers <- price_matrix[,j]*df$Dwellings + price_matrix[,j+10]*df$Dwellings
  df$lower_buyers <- price_matrix[,j]*df$Dwellings - price_matrix[,j+10]*df$Dwellings
  sum_upper <- rbind(sum_upper,sum(df$upper_buyers))
  sum_lower <- rbind(sum_lower,sum(df$lower_buyers))
  sum_buyers <- rbind(sum_buyers,sum(df$buyers))
  running_price <- rbind(running_price,price)
}

pricevurve01 <- cbind(running_price,sum_buyers,sum_upper,sum_lower)
pricevurve01 <- as.data.frame(pricevurve01)
pricevurve01 <- pricevurve01[!(pricevurve01$V1==0),]
pricevurve01$m <- "Atreya"


households_by_price01 <- price_matrix[,1:10]*df$Dwellings

output <- df[c("DAUID")]
output <- cbind(output,households_by_price01)

write.csv(output,file="model01.csv")

#-----------------------------------------
#Price curve
#-----------------------------------------
windowsFonts(Times=windowsFont("TT Times New Roman")) 

overallprices <- rbind(pricevurve01,pricevurve02,pricevurve03,pricevurve04)
overallprices$V2 <- overallprices$V2/1000
ggplot(data = overallprices, aes(x=overallprices$V1, y=overallprices$V2,group=overallprices$m)) + 
  geom_line(aes(color=overallprices$m),size=1) +
  ggtitle("Price and insurance uptake - Calgary") +
  scale_x_continuous(limits = c(0,10)) +
  labs(x = "Price per $1000 coverage") +
  labs(y = "# households covered (thousands)") +
  labs(color="Model") +
  theme(plot.title = element_text(family = "Times", color="#000000", face="bold", size=24, hjust=0)) +
  theme(axis.title = element_text(family = "Times", color="#000000", face="bold", size=14)) +
  theme(legend.title = element_text(family = "Times", color="#000000", face="bold", size=14))


#-----------------------------------------
#Price elasticity (all variables set at their means)
#-----------------------------------------

# Atreya
E_price_atreya <- -0.156
E_income_atreya <- 0.390
E_price_browne <- -0.997
E_income_browne <- 1.506


# 
# 
# #----------------------------------
# # MODEL 5 - Botzen et al., 2012
# # A Tobit model that is trickier to turn into predictions
# #----------------------------------
# 
# #step one is go get the xbeta
# #get the sd
# #them calculate lamda
# #then calculate prediction of WTP
# #convert to probability via...
# expected_damage <- 70500 # 1250 * 56.4
# price <- 1
# 
# 
# df$xbeta <- 0.3627 + 
#   0.9448*rtruncnorm(1543,a=0,b=2.1,mean=0.65,sd=0.48) +
#   0.4185*rtruncnorm(1543,a=0,b=10.8,mean=4.19,sd=2.2) +
#   -1.1128*rtruncnorm(1543,a=0,b=2,mean=0.48,sd=0.5) +
#   -0.9069*rtruncnorm(1543,a=0,b=1.1,mean=0.12,sd=0.33) +  
#   0.0509*log(rtruncnorm(1543,a=0,b=1256583,mean=70383,sd=185400)) +  
#   -1.8662*(rtruncnorm(1543,a=0,b=0.61,mean=0.04,sd=0.19)) +  
#   -0.0747*log(rtruncnorm(1543,a=0,b=10000000,mean=113780,sd=3190695)) +
#   -0.3594*(rtruncnorm(1543,a=0,b=1.9,mean=0.39,sd=0.49)) + 
#   -0.1197*df$E + #experience
#   0.2691*(rtruncnorm(1543,a=0,b=1.1,mean=0.12,sd=0.33)) +
#   -0.4817*(rtruncnorm(1543,a=0,b=6.72,mean=3.24,sd=1.16)) +
#   -0.0004*df$Elevation + #elevation relative to water
#   -0.4873*0 + #ignore for the moment -- protected by dikes
#   -0.0082*df$Distance_m + #distance from water
#   0.7420*0 + # rural area
#   -1.0866*0.5 + #female
#   -0.0089*df$med_age + # age
#   -0.2228*log(df$Dwellval) + # property value
#   -.1597*df$percedu/100 + # percent university educated
#   0.7768*log(df$Income) #income
# sigma <- 0.8
# df$scale <- df$xbeta/sigma
# df$pdf_ <- dnorm(df$scale,mean=0, sd=1)
# df$cdf_ <- pnorm(df$scale,mean=0, sd=1)
# df$lambda <- df$pdf_/df$cdf_
# df$WTP <- df$cdf_ * (df$xbeta + sigma*df$lambda)
# df$WTP <- exp(df$WTP)
# 
# 
# 
# 
# 
# 
# 
# 
# #----------------------------------
# # MODEL 7 - Petrolia 2013
# # Individual
# #----------------------------------
# 
# df$xbeta <- -0.913 +
#   0.005*rtruncnorm(1543,a=0,b=90,mean=6.86,sd=10.39) + #future storms (frequency?)
#   0.58*rtruncnorm(1543,a=0,b=10,mean=3.38,sd=2.25) + #future storm damage
#   0*0 + #risk aversion gain
#   0.096*rtruncnorm(1543,a=0,b=5,mean=2.93,sd=1.36) + #risk aversion loss
#   0.281*rtruncnorm(1543,a=0,b=1,mean=0.58,sd=0.49) + #disaster assistance
#   0.245*rtruncnorm(1543,a=0,b=1,mean=0.67,sd=0.47) + #insurer credibility
#   0.410*rtruncnorm(1543,a=0,b=7,mean=0.09,sd=0.47) + # number of flood events
#   -0.001*0 + #coastal resident
#   -0.007*df$Distance_m/1000 + #distance to water
#   0.143*0 + #mortgage
#   0.738*0 + #SFHA status
#   1.490*0 + #interaction between mortgage and SFHA
#   0.056*df$incomecat + #income
#   0.069*rtruncnorm(1543,a=0,b=1,mean=0.06,sd=0.25)#other property
# -1.974*0 + # Florida 
#   -0.108*0 + #CRS
#   0.181*0 + #interaction between above
#   0.316*rtruncnorm(1543,a=0,b=1,mean=0.1,sd=0.1) + #hispanic 
#   0.068*rtruncnorm(1543,a=0,b=1,mean=0.45,sd=0.05) + #male
#   #kids and mobile home exclude--missing data?
#   df$WTB07 <- pnorm(df$xbeta)
# 
# #----------------------------------
# # MODEL 8 - Ren
# # Individual
# # intercept starts at 1/1+ exp(-0.656)
# #----------------------------------
# 
# df$xbeta <- 1/(1+exp(-0.656)) +
#   0.712*rtruncnorm(1543,a=0,b=1,mean=0.781,sd=0.414) + #male
#   0.044*rtruncnorm(1543,a=18,b=88,mean=46.35,sd=11.75) + #age
#   0.279 +
#   -0.147 +
#   0.022 +
#   0.169 +
#   -0.264 +
#   -0.101 +
#   1.647 +
#   0.010 +
#   -2.548 +
#   2.642 +
#   -0.589 +
#   0.124 +
#   0.236 +
#   0.994
# 
# 
# 
# #----------------------------------
# # Initialization Phase
# #----------------------------------
# no_agents <- sum(df$Dwellings)
# no_locs <- length(df$Dwellings)
# A_attributes <- 1
# L_attributes <- 1
# 
# #Create the agents
# id <- data.frame(seq(1:no_agents))
# a <- data.frame(matrix(runif(A_attributes*no_agents), nrow=no_agents,ncol=A_attributes))
# dfA <- cbind(id,a)
# 
# #come up with dynamic names and rename data accordingly
# s <- seq(1,A_attributes)
# name_list <- paste("A",s, sep="")
# name_listf <- c("AID", name_list)
# names(dfA) <- name_listf
# 
# #Create the landscape within which agents reside
# id <- data.frame(seq(1:no_locs))
# a <- data.frame(matrix(runif(L_attributes*no_locs), nrow=no_locs,ncol=L_attributes))
# dfL <- cbind(id,a)
# 
# #come up with dynamic names and rename data accordingly
# s <- seq(1,L_attributes)
# name_listl <- paste("L",s, sep="")
# name_listlf <- c("LID", name_listl)
# names(dfL) <- name_listlf
# 
# #Assign the landscape to the agents (A1 is the LID where the agent is located)
# dfA$LID <- sample(min(dfL$LID):max(dfL$LID),no_agents,replace=T)
# 
# #---------------------------------------------------------
# # Assign the agents to the landscape (multiple steps...)
# #---------------------------------------------------------
# 
# #get the population count through aggregation
# counts <- aggregate(rep(1, length(paste0(dfA$LID))),by=list(dfA$LID), sum)
# names(counts) <- c("LID","L1")
# 
# #this updates for specific variable using 'match'
# matches <- match(counts$LID, dfL$LID)
# dfL$L1[matches] <- counts$L1
# 
# #get averages of a selection of agent attributes
# agg_list <- c("A1")
# name_list[-1]
# variables <- aggregate(dfA[agg_list], by=list(dfA$LID),FUN=mean)
# names(variables)[names(variables) == "Group.1"] <- "LID"
# aggregated_data <- variables[c("LID",agg_list)]
# 
# #add a prefix so this can be merged back to agents file
# colnames(aggregated_data) <- paste("A_", colnames(aggregated_data), sep="")
# colnames(aggregated_data)[1] <- "LID"
# 
# #merge aggregate data back to locations file
# dfL <- merge(dfL,aggregated_data, by="LID")
# 
# #---------------------------------------------------------
# # Update the agents file
# #---------------------------------------------------------
# 
# dfA <- merge(dfA,dfL, by="LID")
# 
# #-----------------------------------
# #Decision Phase
# #D1 - buy insurance
# #-----------------------------------
# 
# dfA$mu1 <- -2 + dfA$A1 + (dfA$A_A1 - dfA$A1)
# dfA$p1 <- 1/(1 + exp(-dfA$mu1))
# dfA$d1 <- rbinom(no_agents, 1, dfA$p1)
# 
# #Update the agent data based on decisions
# 
# s1d1 <- dfA[dfA$d1==1,]
# s1d1$A1 <- s1d1$A1*0.9
# s2d1 <- dfA[dfA$d1==0,]
# dfA <- rbind(s1d1,s2d1)
# 
# #-----------------------------------
# #Decision Phase
# #D2 - Change residence
# #-----------------------------------


