# Remove all previous data
rm(list=ls(all=T))

# Set working directory 
setwd("D:/MS Agroecology/Multi-State Trial-New")

# Install.packages("tidyverse")
library(tidyverse)

# Read in file
M2020<-read.csv("2020_Meta_MultiState.csv",skip=3, header = TRUE,)
M2019<-read.csv("2019_Meta_MultiState.csv",skip=3, header = TRUE,)

# Summary of Data ####

summary(M2020)
summary(M2019)

# Box plots Y ~ Site 2020 ####

boxplot(M2020$grams.m2~M2020$Site,ylab="Grain Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2020$lbs..A~M2020$Site,ylab="Grain Yield A",xlab="",las=2, na.rm=T)
boxplot(M2020$grams.m2.1~M2020$Site,ylab="Stalk Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2020$lbs..A.1~M2020$Site,ylab="Stalk Yield A",xlab="",las=2, na.rm=T)
boxplot(M2020$X..plants.m2~M2020$Site,ylab="Plant Population at Est m2",xlab="",las=2, na.rm=T)
boxplot(M2020$plants.A~M2020$Site,ylab="Plant Population at Est A",xlab="",las=2, na.rm=T)
boxplot(M2020$X..plantsm2~M2020$Site,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2020$plants.A.1~M2020$Site,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2020$Mean..cm.~M2020$Site,ylab="Total Plant Height cm",xlab="",las=2, na.rm=T)
boxplot(M2020$Mean..mm.~M2020$Site,ylab="Stem Diameter",xlab="",las=2, na.rm=T)

# Box plots Y ~ Site 2019 ####

boxplot(M2019$grams.m2~M2019$Site,ylab="Grain Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2019$lbs..A~M2019$Site,ylab="Grain Yield A",xlab="",las=2, na.rm=T)
boxplot(M2019$grams.m2.1~M2019$Site,ylab="Stalk Yield m2",xlab="",las=2, na.rm=T)
## NA's : boxplot(M2019$lbs..A.1~M2019$Site,ylab="Stalk Yield A",xlab="",las=2, na.rm=T)
boxplot(M2019$X..plants.m2~M2019$Site,ylab="Plant Population at Est m2",xlab="",las=2, na.rm=T)
## NA's : boxplot(M2019$plants.A~M2019$Site,ylab="Plant Population at Est A",xlab="",las=2, na.rm=T)
boxplot(M2019$X..plantsm2~M2019$Site,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
## NA's boxplot(M2019$plants.A.1~M2019$Site,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2019$Mean..cm.~M2019$Site,ylab="Total Plant Height cm",xlab="",las=2, na.rm=T)
## NA's (M2019$Mean..mm.~M2019$Site,ylab="Stem Diameter",xlab="",las=2, na.rm=T)

# Box plots Y ~ Variety 2020 ####

boxplot(M2020$grams.m2~M2020$Variety...Line,ylab="Grain Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2020$lbs..A~M2020$Variety...Line,ylab="Grain Yield A",xlab="",las=2, na.rm=T)
boxplot(M2020$grams.m2.1~M2020$Variety...Line,ylab="Stalk Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2020$lbs..A.1~M2020$Variety...Line,ylab="Stalk Yield A",xlab="",las=2, na.rm=T)
boxplot(M2020$X..plants.m2~M2020$Variety...Line,ylab="Plant Population at Est m2",xlab="",las=2, na.rm=T)
boxplot(M2020$plants.A~M2020$Variety...Line,ylab="Plant Population at Est A",xlab="",las=2, na.rm=T)
boxplot(M2020$X..plantsm2~M2020$Variety...Line,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2020$plants.A.1~M2020$Variety...Line,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2020$Mean..cm.~M2020$Variety...Line,ylab="Total Plant Height cm",xlab="",las=2, na.rm=T)
boxplot(M2020$Mean..mm.~M2020$Variety...Line,ylab="Stem Diameter",xlab="",las=2, na.rm=T)

# Box plots Y ~ Variety 2019 ####
 
boxplot(M2019$grams.m2~M2019$Variety...Line,ylab="Grain Yield m2",xlab="",las=2, na.rm=T)
boxplot(M2019$lbs..A~M2019$Variety...Line,ylab="Grain Yield A",xlab="",las=2, na.rm=T)
boxplot(M2019$grams.m2.1~M2019$Variety...Line,ylab="Stalk Yield m2",xlab="",las=2, na.rm=T)
## NA's :boxplot(M2019$lbs..A.1~M2019$Variety...Line,ylab="Stalk Yield A",xlab="",las=2, na.rm=T)
boxplot(M2019$X..plants.m2~M2019$Variety...Line,ylab="Plant Population at Est m2",xlab="",las=2, na.rm=T)
## NA's boxplot(M2019$plants.A~M2019$Variety...Line,ylab="Plant Population at Est A",xlab="",las=2, na.rm=T)
boxplot(M2019$X..plantsm2~M2019$Variety...Line,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
## NA's boxplot(M2019$plants.A.1~M2019$Variety...Line,ylab="Plant Population at Harv m2",xlab="",las=2, na.rm=T)
boxplot(M2019$Mean..cm.~M2019$Variety...Line,ylab="Total Plant Height cm",xlab="",las=2, na.rm=T)
## NA's boxplot(M2019$Mean..mm.~M2019$Variety...Line,ylab="Stem Diameter",xlab="",las=2, na.rm=T)

## GG Plots Y ~ Sites 2020 ####

ggplot(M2020, aes(Site,grams.m2,colour=Site,))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,lbs..A,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,grams.m2.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,lbs..A.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,X..plants.m2,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,plants.A,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,X..plantsm2,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,plants.A.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,Mean..cm.,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2020, aes(Site,Mean..mm.,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

## GG Plots Y ~ Sites 2019 ####

ggplot(M2019, aes(Site,grams.m2,colour=Site,))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,lbs..A,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,grams.m2.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,lbs..A.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,X..plants.m2,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,plants.A,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Site,X..plantsm2,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,plants.A.1,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,Mean..cm.,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

ggplot(M2019, aes(Site,Mean..mm.,colour=Site))+
  geom_point(size=3)+
  geom_line(color="red")

## GG plots Y ~ Variety 2020 ####

ggplot(M2020, aes(Variety...Line,grams.m2,colour=Variety...Line,))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,lbs..A,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,grams.m2.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,lbs..A.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,X..plants.m2,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,plants.A,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,X..plantsm2,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,plants.A.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,Mean..cm.,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2020, aes(Variety...Line,Mean..mm.,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")


## GG plots Y ~ Variety 2019 ####

ggplot(M2019, aes(Variety...Line,grams.m2,colour=Variety...Line,))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,lbs..A,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,grams.m2.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,lbs..A.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,X..plants.m2,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,plants.A,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,X..plantsm2,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,plants.A.1,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,Mean..cm.,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")
ggplot(M2019, aes(Variety...Line,Mean..mm.,colour=Variety...Line))+
  geom_point(size=3)+
  geom_line(color="red")

## ANOVA's 2020 ####

aov20Grainm2<-aov(grams.m2~Site,data=M2020)
summary(aov20Grainm2)
plot(aov20Grainm2)

aov20GrainA<-aov(lbs..A~Site,data=M2020)
summary(aov20GrainA)
plot(aov20GrainA)

aov20Stalkm2<-aov(grams.m2.1~Site,data=M2020)
summary(aov20Stalkm2)
plot(aov20Stalkm2)

aov20StalkA<-aov(lbs..A.1~Site,data=M2020)
summary(aov20StalkA)
plot(aov20StalkA)

aov20PPEm2<-aov(X..plants.m2~Site,data=M2020)
summary(aov20PPEm2)
plot(aov20PPEm2)

## aov20PPEA<-aov(plants.A~Site,data=M2020)
# summary(aov20PPEA)
# plot(aov20PPEA)

aov20PPHm2<-aov(X..plantsm2~Site,data=M2020)
summary(aov20PPHm2)
plot(aov20PPHm2)

aov20PPHA<-aov(plants.A.1~Site,data=M2020)
summary(aov20PPHA)
plot(aov20PPHA)

aov20Height<-aov(Mean..cm.~Site,data=M2020)
summary(aov20Height)
plot(aov20Height)

aov20Dia<-aov(Mean..mm.~Site,data=M2020)
summary(aov20Dia)
plot(aov20Dia)

## ANOVA's 2019 ####

aov19Grainm2<-aov(grams.m2~Site,data=M2019)
summary(aovGrainm2)
plot(aovGrainm2)

aov19GrainA<-aov(lbs..A~Site,data=M2019)
summary(aovGrainA)
plot(aovGrainA)

aov19Stalkm2<-aov(grams.m2.1~Site,data=M2019)
summary(aovStalkm2)
plot(aovStalkm2)

#aov19StalkA<-aov(lbs..A.1~Site,data=M2019)
#summary(aovStalkA)
#plot(aovStalkA)

aov19PPEm2<-aov(X..plants.m2~Site,data=M2019)
summary(aov19PPEm2)
plot(aov19PPEm2)

#aov19PPEA<-aov(plants.A~Site,data=M2019)
#summary(aov19PPEA)
#plot(aov19PPEA)

aov19PPHm2<-aov(X..plantsm2~Site,data=M2019)
summary(aov19PPHm2)
plot(aovStalkA)

#aov19PPHA<-aov(plants.A.1~Site,data=M2019)
#summary(aov19PPHA)
#plot(aov19PPHA)

aov19Height<-aov(Mean..cm.~Site,data=M2019)
summary(aov19Height)
plot(aov19Height)

#aov19Dia<-aov(Mean..mm.~Site,data=M2019)
#summary(aov19Dia)
#plot(aov19Dia)