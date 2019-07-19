###### script3_Figure1A

###Space set up
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

##import data and packages
data<-read.csv("./Data/2016_2017_phenology_percentage_cover.csv")
library(dplyr)
library(ggplot2)
library(gtable)
library(gridExtra)
library(grid)


#### Narrowing to one site where both species occur and to only flower observations
data<-data[data$sp.cover.in.plot!="NA",]
lowdata<-data[data$Site=="Low",]
lowdata<-lowdata[lowdata$Phenological.event=="Flower",]

#making seperate datasets for Calluna and Dracophyllum
low<-lowdata[lowdata$Species=="CALVUL",]
low2dr<-lowdata[lowdata$Species=="DRASUB",]
lowdr<-low2dr[low2dr$Removal=="C",]


### Calculating means by species by observation date
cvmean_data<- group_by(low, Days, Warmed) %>%
  summarise(events.per.m2 = mean(events.per.m2, na.rm = TRUE))
drmean_data<- group_by(lowdr, Days, Warmed) %>%
  summarise(events.per.m2 = mean(events.per.m2, na.rm = TRUE))



##format data
drmean_data$Species<-rep("DR", nrow(drmean_data))
cvmean_data$Species<-rep("CV", nrow(cvmean_data))

warmdr<-drmean_data[drmean_data$Warmed=="W",]
warmcv<-cvmean_data[cvmean_data$Warmed=="W",]
warm_mean<-rbind(warmcv,warmdr)

ambdr<-drmean_data[drmean_data$Warmed=="A",]
ambcv<-cvmean_data[cvmean_data$Warmed=="A",]
amb_mean<-rbind(ambcv,ambdr)


########################### FINAL PLOTS
######ambient conditions

ylab <- expression(paste("Flowers m"^ "-1"))
cvlab <- expression(paste(italic("Calluna vulgaris")))
drlab <- expression(paste(italic("Dracophyllum subulatum")))

p4<-ggplot(na.omit(amb_mean), aes(x = Days, y =events.per.m2)) +
  geom_point(size=8) + geom_line(size=3, aes(linetype=Species)) +
  ylab(ylab)+ xlab("Days from onset of astronomical spring ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+  
  scale_x_continuous(limits=c(63, 188), breaks=seq(60,190,20))+
  scale_y_continuous(trans='log2', breaks=c(2, 20, 200, 2000)) +
  scale_linetype_manual(values= c("twodash", "solid"), labels=c(cvlab, drlab))+
  geom_ribbon(data=subset(amb_mean,Species=="DR"& Days>149),
              aes(x=Days,ymax=events.per.m2),ymin=-1,fill="steelblue3", alpha=0.2, linetype="blank")+
  theme(legend.position="top")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  geom_segment(x=150, xend=150, y=-1, yend= 10.2, colour="steelblue3", size=2)+
  annotate(geom="text", x=72, y=2000, label="Ambient",color="steelblue3", size=15)+
  theme(text = element_text(size=50))+
  theme(axis.line.x = element_line(color="black", size = 2),
        axis.line.y = element_line(color="black", size = 2))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)))


######warmed conditions

p3<-ggplot(na.omit(warm_mean), aes(x = Days, y =events.per.m2)) +
  geom_point(size=8) + geom_line(size=3, aes(linetype=Species)) +
  ylab(ylab)+ xlab("Days past astronomical spring ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+  
   scale_x_continuous(limits=c(63, 188), breaks=c(70,90,104,122,138,150,165,173,187))+
  scale_y_continuous(trans='log2', breaks=c(2, 20, 200, 2000)) +
  scale_linetype_manual(values= c("twodash", "solid"), labels=c(cvlab, drlab))+
  geom_ribbon(data=subset(warm_mean,Species=="DR"& Days > 125),
              aes(x=Days,ymax=events.per.m2),ymin=-1,fill="tomato2", alpha=0.2, linetype="blank")+
  theme(legend.position="none")+
  geom_segment(x=138, xend=138, y=-1, yend= 10.8, colour="tomato2", size=2)+
 annotate(geom="text", x=85, y=2000, label="Experimental warming",color="tomato2", size=15)+
  theme(text = element_text(size=50))+
  theme(axis.line.x = element_line(color="black", size = 2),
        axis.line.y = element_line(color="black", size = 2))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 25, l = 0)))

grid.arrange(p4, p3)

#### UNNECESSARY


cvylab <- expression(paste(italic("Calluna vulgaris"), " flowers m"^ "-1"))


p1<-ggplot(na.omit(cvmean_data), aes(x = Days, y =events.per.m2, colour = Warmed)) +
  geom_point() + geom_line(size=1.5)+
  geom_point(data=low, aes(x = Days,y =events.per.m2, colour = Warmed)) +
  ylab(cvylab)+scale_x_continuous(limits=c(63, 188))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+  scale_y_continuous(trans='log2', breaks=c(1,10, 100, 1000, 10000)) + 
  scale_colour_manual(values=c("steelblue3", "tomato2"), name="", labels=c("Control", "Experimental warming"))+ 
  geom_segment(x=138, xend=138, y=-5, yend= 11, colour="tomato2", size=1, linetype="longdash")+ 
  geom_segment(x=150, xend=150, y=-1, yend= 10.2, colour="steelblue3",  size=1, linetype="longdash")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  theme(plot.title = element_text(face="italic"))+ 
  theme(legend.position="top")

dsylab <- expression(paste(italic("Dracophyllum subulatum"), " flowers m"^ "-1"))
p2<-ggplot(na.omit(cvmean_data), aes(x = Days, y =events.per.m2, colour = Warmed)) +
  geom_point() + geom_line(size=1.5)+
  geom_point(data=low, aes(x = Days,y =events.per.m2, colour = Warmed)) +
  ylab(dsylab)+ xlab("Days from onset of astronomical spring ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+  
  scale_y_continuous(trans='log2', breaks=c(1,10, 100, 1000)) + scale_x_continuous(limits=c(63, 188), breaks=seq(60,190,20))+
  scale_colour_manual(values=c("steelblue3", "tomato2"))+
  geom_ribbon(data=subset(cvmean_data,Warmed=="W" & Days>137),
              aes(x=Days,ymax=events.per.m2),ymin=-1,fill="tomato2", alpha=0.2, linetype="blank")+
  geom_ribbon(data=subset(cvmean_data,Warmed=="A" & Days>149),
              aes(x=Days,ymax=events.per.m2),ymin=-1,fill="steelblue3", alpha=0.2, linetype="blank")+ 
  geom_vline(xintercept=138.1, colour="tomato2", size=1, linetype="longdash")+ 
  geom_vline(xintercept=150.1, colour="steelblue3", size=1, linetype="longdash")+ 
  theme(plot.title = element_text(face="italic"))+ 
  theme(legend.position="none")



grid.arrange(p1, p2)



       