#PUMF analysis

df <- data.frame(read.csv(file="D:\\ALLWORK\\PROJECTS\\Floodnet\\StatsData\\CalgaryNHS2011Microdata.csv", header=T))

df2 <- df[df$TOTINC != 9999999 & df$TOTINC != 8888888 & df$value != 9999999 & df$value != 8888888,]