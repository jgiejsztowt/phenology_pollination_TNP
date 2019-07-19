###treatment effect in OTCs 4 oct 2018

#set up space
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

#import data and packages
library(nlme)
library(multcomp)
library(ggplot2)

airtemp<-read.csv("./Data/WaRM ibutton data.csv") ##data=airtemp only, low elevation only 


at<-airtemp[airtemp$Air_Temp_C<85,]  ####removing data points outside of thermal tolerance of thermochron DS1922L
at<-at[at$Air_Temp_C>-40,]  #https://www.maximintegrated.com/en/app-notes/index.mvp/id/3892
nrow(airtemp); nrow(at)
at$td<-paste(at$Date,at$Time)

###linear mixed effects model as per suonan 2017
lm1<-lme(Air_Temp_C~Warmed, random=~1|td, data=at)  ##treatment as fixed effect, time as random effect
summary(lm1)
anova(lm1)

#########################################################
#############plots#######################################
#########################################################

at<-at[at$Season=="Growing",]
y2016<-at[at$Year==2015,]
y2016ag <- aggregate(y2016$Air_Temp_C, by=list(y2016$Date, y2016$Warmed), FUN=mean)
colnames(y2016ag) <- c("Date", "Warmed", "Temp")
y2016ag$Date2<-as.Date(y2016ag$Date, format = "%d/%m/%Y")


ylab <- expression('Temperature ('*~degree*C*')')

ggplot(data= y2016ag, aes(x=Date2, y=Temp, group=Warmed, colour=Warmed)) + 
  geom_line(lwd=2) + 
  scale_colour_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming")) + 
  theme_light()+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+
  theme(text = element_text(size=30))+
  ylab(ylab)+
  theme(axis.title.x=element_blank())



