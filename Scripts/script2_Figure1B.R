#####script2_Figure 1B 


#set up space

rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

#import data and packages
sw<-read.csv("./Data/seed weights.csv")
library(ggplot2)


########adjustment of datasets for plant height

sw$weight.adj<- 0
for (i in 1:nrow(sw)) {
  n <-  log(as.numeric(sw[i,6]))-0.0005004*sw[i,7] 
  sw$weight.adj[i] <- n
} ###the values here are derived from script1__seedweight



########reformatting dataset

n<-sw[sw$Treatment=="N",] #limiting data to just "natural pollination" treatment 
n$Proportion.native<-as.numeric(as.character(n$Proportion.native))
n$Average.number.of.flowers<-as.numeric(as.character(n$Average.number.of.flowers.on.25m.transect))
n$floral.densitym2<-n$Average.number.of.flowers/10 #original data was in flowers/decimetre

str(n)
n$prop.nat.bin<-0  ##binary version of native:invasive ratio data
n[n$Proportion.native>0.5,18]<-rep("Native dominance",length(n[n$Proportion.native>0.5,14]))
n[n$Proportion.native<0.5,18]<-rep("Invasive dominance",length(n[n$Proportion.native<0.5,14]))

########creating plot

xlab<-expression("Landscape floral density (Flowers m"^"-1"*")")
ylab<-expression(paste("Adjusted seed weight (log(g))"))

ggplot(n,aes(y = weight.adj, x = floral.densitym2, group = prop.nat.bin)) +
  geom_point(size=8,aes(col=prop.nat.bin)) + 
  geom_smooth(method = "lm", fill = NA, lwd=3,fullrange=TRUE, linetype="twodash", aes(col = prop.nat.bin))+
  xlab(xlab)+
  ylab(ylab)+
  theme(legend.position="top")+
  theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(text = element_text(size=50))+
  labs(colour="Floral landscape: ") +
  scale_color_manual(values=c("grey30", "olivedrab3"))+
  theme(axis.line.x = element_line(color="black", size = 2),
        axis.line.y = element_line(color="black", size = 2))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0))) ####export at size=250