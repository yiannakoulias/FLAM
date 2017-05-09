#PUMF analysis

df <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\StatsData\\CalgaryNHS2011Microdata.csv", header=T))

#keep reasonable values
df <- df[which(df$VALUE > 100000 & df$TOTINC <= 100000 & df$VALUE <= 1000000),]

#clearly the NHS PUMF data are a disaster...!
plot(df[df$AGEGRP < 24,]$AGEGRP, df[df$AGEGRP < 24,]$TOTINC)

summary(lm(data=df, VALUE ~ TOTINC))

