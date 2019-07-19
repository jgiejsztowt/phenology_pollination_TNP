#Script4_ supplementary figure 8: seed weights among treatments

### set up space
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")


#### packages and data
library("ggplot2")
sw<-read.csv("./Data/seed weights.csv")


#############################################################
############# correcting for plant height

sw$weight.adj<- 0

for (i in 1:nrow(sw)) {
  n <-  log(as.numeric(sw[i,6]))-0.0005004*sw[i,7]
  sw$weight.adj[i] <- n
} ###adjusting for plant height, as defined  in script_1


ggplot(sw, aes(x=Treatment, y=weight.adj)) + 
  geom_boxplot(fill="#999999", color="black", lwd=1)+ 
  theme_light()+
  scale_x_discrete(labels=c("Pollinator exclusion","Hand pollination", "Natural pollination"))+
  xlab("Treatment")+
  ylab("Seed weight log(g)")+
  geom_text(x=1, y=-5, label="a", size=15)+
  geom_text(x=2, y=-5, label="b", size=15)+
  geom_text(x=3, y=-5, label="b", size=15)+
  scale_y_continuous(limits = c(-7.5,-4.5))+ 
  theme(text = element_text(size=30))




#############################################################
############# statistical analyses
###1 way ANOVA
res.aov <- aov(weight.adj ~ Treatment, data = sw);summary(res.aov)
TukeyHSD(res.aov)

