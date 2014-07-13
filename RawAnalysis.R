stormData <- read.csv("/Users/gaspar/Dropbox/Cursos/Coursera/ReproducibleResearch/Assessment2/repdata-data-StormData.csv.bz2", header=TRUE, fill=TRUE, na.strings="NA")
head(stormData, n=0L)
summary(stormData$EVTYPE)
table(stormData$PROPDMG)
table(stormData$INJURIES)
plot(stormData$FATALITIES~stormData$EVTYPE)
plot(stormData$INJURIES~stormData$EVTYPE)
temp <- aggregate(stormData$FATALITIES, list(stormData$EVTYPE), FUN=sum)
temp2 <- aggregate(stormData$INJURIES, list(stormData$EVTYPE), FUN=sum)
plot(temp$x~temp$Group.1, type="b")
fatalities <- subset(temp, temp$x > 1)
injuries <- subset(temp2, temp2$x != 0)
plot(fatalities$Group.1, fatalities$x, type="h")
plot(injuries$Group.1, injuries$x, type="l")
temp$percent<-100*(temp$x/sum(temp$x))
plot(temp$percent, type="l")
temp2$percent <- 100*(temp2$x/sum(temp$x))
plot(temp2$percent, type="l")
tempNOtornado <- subset(temp, temp$x < 20)
plot(tempNOtornado$x, type="l")
tempProblem <- subset(tempNOtornado, tempNOtornado$x > 10)
tempProblem
rm(tempNOtornado, tempProblem)
temp <- aggregate(stormData$FATALITIES, list(stormData$EVTYPE), FUN=sum)
temp2 <- aggregate(stormData$INJURIES, list(stormData$EVTYPE), FUN=sum)
temp3 <- as.data.frame(table(stormData$EVTYPE))
str(temp)
str(temp2)
names(temp)[2] <- "nfatalities"
names(temp)[3] <- "pfatalities"
names(temp2)[2] <- "ninjuries"
names(temp2)[3] <- "pinjuries"

prueba <- merge(temp, temp2, by="Group.1")
str(prueba)

str(temp3)
noaa <- merge(prueba,temp3, by.x="Group.1", by.y="Var1")
str(noaa)
tail(noaa)
rm(prueba)

noaa$percent <- noaa$Freq/sum(noaa$Freq)*100
sum(noaa$percent)

plot(noaa$percent, type="l")
plot(noaa$percent, noaa$pfatalities)
plot(noaa$percent, stormData$PROPDMG)
identify(noaa$percent, noaa$pfatalities, labels=noaa$Group.1)

library("ggplot2")
ggplot(noaa, aes(x = percent, y = pfatalities)) + geom_point(shape = 19)+labs(x ="Frequency of the event as a percentage of the total of events", y = "Percentage of fatalities  in 61 years of records (%)")+ggtitle("Fatality of the weather events")

subset(noaa, noaa$pfatalities > 5)
##################
# Load raw data
stormData <- read.csv("/Users/gaspar/Dropbox/Cursos/Coursera/ReproducibleResearch/Assessment2/RepData_PeerAssessment2/repdata-data-StormData.csv.bz2", header=TRUE, fill=TRUE, na.strings="NA")
# Explore the 37 variables
head(stormData, n=0L)
temp <- aggregate(stormData$FATALITIES, list(stormData$EVTYPE), FUN=sum)
temp$percent<-100*(temp$x/sum(temp$x)) # Aggregate a column with the percentage
sum(temp$percent)
temp2 <- aggregate(stormData$PROPDMG, list(stormData$EVTYPE), FUN=sum)
temp2$percent <- 100*(temp2$x/sum(temp2$x))
sum(temp2$percent)
temp3 <- as.data.frame(table(stormData$EVTYPE))
names(temp)[2] <- "nfatalities"
names(temp)[3] <- "pfatalities"
names(temp2)[2] <- "nproperties"
names(temp2)[3] <- "pproperties"
# I merged the data to construct a dataset that contains all the data we're interested in
prueba <- merge(temp, temp2, by="Group.1")
noaa <- merge(prueba,temp3, by.x="Group.1", by.y="Var1")
noaa$percent <- noaa$Freq/sum(noaa$Freq)*100


ggplot(noaa, aes(x = percent, y = pproperties)) + geom_point(shape = 19)+labs(x ="Frequency of the event as a percentage of the total of events", y = "Percentage of propertie damage in 61 years of records (%)")+ggtitle("Propertie damage caused by weather events")
# I tidy a little bit by giving proper names to the variables
names(temp)[2] <- "nfatalities"
names(temp)[3] <- "pfatalities"
names(temp2)[2] <- "nproperties"
names(temp2)[3] <- "pproperties"
# I merged the data to construct a dataset that contains all the data we're interested in
prueba <- merge(temp, temp2, by="Group.1")
noaa <- merge(prueba,temp3, by.x="Group.1", by.y="Var1")
noaa$percent <- noaa$Freq/sum(noaa$Freq)*100